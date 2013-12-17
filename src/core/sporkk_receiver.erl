%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc The bot's receiver process.
%% 		Handles connecting to IRC, receiving events from the IRC connection
%% 		and passing them to the event manager.
%% ============================================================================

-module(sporkk_receiver).
-behavior(gen_fsm).
-include("sporkk.hrl").

%% API Functions
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, connecting/2, registering/2, running/2, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% State record
-record(state, {
		  botid,
		  nick,
		  account_map
		 }).


%% ============================================================================
%% API Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the receiver.
%% ----------------------------------------------------------------------------
start_link(BotId) ->
	gen_fsm:start_link(sporkk:receiver(BotId), ?MODULE, [BotId], []).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Initializes the server's state.
%% ----------------------------------------------------------------------------
init([BotId]) ->
	{ok, Bot} = sporkk_cfg:get_bot(BotId),
	{ok, connecting, #state{botid=BotId, nick=Bot#bot.nick, account_map=ets:new(receiver_account_map, [private])}}.


%% ----------------------------------------------------------------------------
%% @doc Connecting state, in which the core waits for the connection to the
%% 		IRC server to be made.
%% ----------------------------------------------------------------------------
connecting(connected, State) ->
	error_logger:info_msg("TCP connection success. Registering with the IRC server."),
	BotId = State#state.botid,
	{ok, Bot} = sporkk_cfg:get_bot(BotId),
	ok = gen_server:cast(sporkk:sender(BotId), {register, Bot#bot.nick}),
	{next_state, registering, State};
connecting(_Request, State) ->
	{next_state, connecting, State}.

%% ----------------------------------------------------------------------------
%% @doc Registering state, in which the core logs in to the IRC server.
%% ----------------------------------------------------------------------------
registering({recv, {DateTime, LineData}}, State) ->
	BotId = State#state.botid,
	{ok, Bot} = sporkk_cfg:get_bot(BotId),
	{ok, Line} = parse_line(Bot, DateTime, LineData),
	case Line#line.operation of
		ping ->
			ok = gen_server:cast(sporkk:sender(BotId), {pong, Line#line.body}),
			{next_state, registering, State};
		err_nicknameinuse ->
			% TODO: Get the nick from the database, not the local state. This may cause issues.
			error_logger:info_msg("Nick in use. Trying another one."),
			NewNick = Bot#bot.nick ++ "_",
			ok = gen_server:cast(sporkk:sender(BotId), {nick, NewNick}),
			{next_state, registering, State#state{nick=NewNick}};
		rpl_welcome ->
			error_logger:info_msg("Connected to IRC. Joining channels."),
			Channels = Bot#bot.channels,
			ok = gen_server:cast(sporkk:sender(BotId), {join, Channels}),
			% TODO: Send Lines to the event manager.
			%ok = gen_server:cast(, {line, Line}),
			{next_state, running, State};
		_ ->
			{next_state, registering, State}
	end.

%% ----------------------------------------------------------------------------
%% @doc Running state, in which the core handles messages from the IRC server.
%% ----------------------------------------------------------------------------
running({recv, {DateTime, LineData}}, State) ->
	BotId = State#state.botid,
	{ok, Bot} = sporkk_cfg:get_bot(BotId),
	{ok, Line} = parse_line(Bot, DateTime, LineData),
	Result = case Line#line.operation of
				 ping ->
					 ok = gen_server:cast(sporkk:sender(BotId), {pong, Line#line.body}),
					 {next_state, running, State};
				 nickchanged ->
					 {next_state, running, State#state{nick=Line#line.body}};

				 join ->
					 case string:tokens(Line#line.origin, "!@") of
						 [Nick, _User, _Host] ->
							 gen_server:cast(sporkk:sender(BotId), {whois, Nick});
						 _ ->
							 pass
					 end,
					 {next_state, running, State};
				 rpl_whoisaccount ->
					 [Nick, Account] = Line#line.options,
					 true = ets:insert(State#state.account_map, {Nick, Account}),
					 {next_state, running, State};
				 part ->
					 case string:tokens(Line#line.origin, "!@") of
						 [Nick, _User, _Host] ->
							 true = ets:delete(State#state.account_map, Nick);
						 _ ->
							 pass
					 end,
					 {next_state, running, State};

				 _ ->
					 {next_state, running, State}
			 end,
	
	% Fill out the account field on the line data.
	NewLineData = case Line#line.origin of
					  undefined ->
						  Line;
					  _ ->
						  case string:tokens(Line#line.origin, "!@") of
							  [Nick2, _User2, _Host2] ->
								  case ets:lookup(State#state.account_map, Nick2) of
									  [{_, Acct}] ->
										  Line#line{account=Acct};
									  _ ->
										  Line
								  end;
							  _ ->
								  Line
						  end
				  end,

	% TODO: Send events to the event manager.
	%gen_server:cast(, {line, NewLineData}),

	Result.

% Ignore these.
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%% ----------------------------------------------------------------------------
%% @doc Handle non cast/call messages.
%% ----------------------------------------------------------------------------
handle_info(_Request, StateName, State) ->
	{noreply, StateName, State}.


%% ----------------------------------------------------------------------------
%% @doc Called when the server terminates.
%% ----------------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
	ok.

%% ----------------------------------------------------------------------------
%% @doc Called when code changes.
%% ----------------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.


%% ============================================================================
%% Internal Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Parses a line from the IRC server.
%% ----------------------------------------------------------------------------
parse_line(Bot, DateTime, [$: | Line]) ->
	BodyPos = string:chr(Line, $:),
	Data = case BodyPos > 0 of
			   true ->
				   Header = string:substr(Line, 1, BodyPos - 1),
				   Body = string:substr(Line, BodyPos + 1),
				   HeaderBits = string:tokens(Header, " "),

				   case length(HeaderBits) of
					   1 ->
						   #line{
							  bot = Bot,
							  datetime = DateTime,
							  origin = lists:nth(1, HeaderBits),
							  body = Body
							 };
					   2 ->
						   #line{
							  bot = Bot,
							  datetime = DateTime,
							  origin = lists:nth(1, HeaderBits),
							  operation = irc_lib:operation_to_atom(lists:nth(2, HeaderBits)),
							  body = Body
							 };
					   _ ->
						   #line{
							  bot = Bot, 
							  datetime = DateTime,
							  origin = lists:nth(1, HeaderBits),
							  operation = irc_lib:operation_to_atom(lists:nth(2, HeaderBits)),
							  destination = lists:nth(3, HeaderBits),
							  options = lists:nthtail(3, HeaderBits),
							  body = Body
							 }
				   end;
			   false ->
				   [Origin, Operation, Destination | _] = string:tokens(Line, " "),
				   #line{
					  bot = Bot,
					  datetime = DateTime,
					  origin = Origin,
					  operation = irc_lib:operation_to_atom(Operation),
					  destination = Destination,
					  body = ""
					 }
		   end,
	{ok, Data};

parse_line(Bot, DateTime, [$P, $I, $N, $G, $\s, $: | Server]) ->
	{ok,
	 #line{
		bot = Bot,
	    datetime = DateTime,
	    operation = ping,
	    body = Server
	   }
	};

parse_line(_Bot, _DateTime, _Data) ->
	{error, malformed_line}.


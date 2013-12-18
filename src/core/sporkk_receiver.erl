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
	{ok, Line} = irc_lib:parse_message(Bot, DateTime, LineData),
	case Line#line.command of
		ping ->
			ok = gen_server:cast(sporkk:sender(BotId), {pong, Line#line.body}),
			{next_state, registering, State};
		err_nicknameinuse ->
			% TODO: Get the nick from the database, not the local state. This may cause issues.
			error_logger:info_msg("Nick in use. Trying another one."),
			NewNick = Bot#bot.nick ++ "_",
			ok = gen_server:cast(sporkk:sender(BotId), {nick, NewNick}),
			{next_state, registering, State#state{nick=NewNick}};
		reply_welcome ->
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
	{ok, BareLine} = irc_lib:parse_message(Bot, DateTime, LineData),

	% Fill out the account field on the line data.
	Line = case BareLine#line.source of
			   undefined ->
				   BareLine;
			   _ ->
				   case string:tokens(BareLine#line.source, "!@") of
					   [Nick2, _User2, _Host2] ->
						   case ets:lookup(State#state.account_map, Nick2) of
							   [{UserNick, Acct}] ->
								   BareLine#line{user={UserNick, Acct}};
							   _ ->
								   BareLine
						   end;
					   _ ->
						   BareLine
				   end
		   end,

	case Line#line.command of
		ping ->
			ok = gen_server:cast(sporkk:sender(BotId), {pong, Line#line.body}),
			{next_state, running, State};
		nickchanged ->
			{next_state, running, State#state{nick=Line#line.body}};

		join ->
			case string:tokens(Line#line.source, "!@") of
				[Nick, _User, _Host] ->
					gen_server:cast(sporkk:sender(BotId), {whois, Nick});
				_ ->
					pass
			end,
			{next_state, running, State};
		reply_whoisaccount ->
			[Nick, Account] = Line#line.args,
			true = ets:insert(State#state.account_map, {Nick, Account}),
			{next_state, running, State};
		part ->
			case string:tokens(Line#line.source, "!@") of
				[Nick, _User, _Host] ->
					true = ets:delete(State#state.account_map, Nick);
				_ ->
					pass
			end,
			{next_state, running, State};

		privmsg ->
			% TODO: Parse the message to see if it's a command.
			% Determine where the message was sent to (was it a channel message or a PM?)
			Source = case Line#line.destination of
						 [$#|_Channel] ->
							 {chan, Line#line.destination};
						 _ ->
							 % TODO: Recognize PMs.
							 undefined
					 end,
			
			% Send the message to the event manager.
			gen_event:notify(sporkk:eventmgr(BotId), {message, {Source, Line#line.user, Line#line.body}}),

			% Continue running.
			{next_state, running, State};

		_ ->
			{next_state, running, State}
	end.

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


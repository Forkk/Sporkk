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
	{ok, connecting, #state{botid=BotId, nick=Bot#bot.nick}}.


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
	{ok, Line} = irc_lib:parse_message(Bot, DateTime, LineData),

	case Line#line.command of
		ping ->
			ok = gen_server:cast(sporkk:sender(BotId), {pong, Line#line.body}),
			{next_state, running, State};

		nickchanged ->
			{next_state, running, State#state{nick=Line#line.body}};

		privmsg ->
			% Send the message to the module server for processing.
			gen_server:cast(sporkk:modserv(BotId), {event, message, {Line#line.dest, Line#line.sender, Line#line.body}}),

			% Figure out if it's a command.
			Body = Line#line.body,
			% TODO: Allow changing the command prefix.
			% TODO: Allow using the bot's nick as a command prefix.
			PfxPos = string:chr(Body, $.),
			if
				PfxPos =:= 1, length(Body) > 1 ->
					CmdMsg = string:right(Body, length(Body)-1),
					% Don't bother parsing the command. We'll let the module server handle that.
					gen_server:cast(sporkk:modserv(BotId), {command, Line#line.dest, Line#line.sender, CmdMsg});

				true ->
					pass
			end,

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


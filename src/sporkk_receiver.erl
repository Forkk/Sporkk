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
		  alt_nicks=[],
		  nick=none
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
	{ok, connecting, #state{botid=BotId}}.


%% ----------------------------------------------------------------------------
%% @doc Connecting state, in which the core waits for the connection to the
%% 		IRC server to be made.
%% ----------------------------------------------------------------------------
connecting(connected, State) ->
	error_logger:info_msg("TCP connection success. Registering with the IRC server."),
	{next_state, registering, set_alt_nicks(State)};
connecting(_Request, State) ->
	{next_state, connecting, State}.

%% ----------------------------------------------------------------------------
%% @doc Registering state, in which the core logs in to the IRC server.
%% ----------------------------------------------------------------------------
registering({recv, {DateTime, LineData}}, State) ->
	BotId = State#state.botid,
	Bot = sporkk_cfg:get_bot(BotId),
	{ok, Line} = irc_lib:parse_message(Bot, DateTime, LineData),
	case Line#line.command of
		ping ->
			ok = gen_server:cast(sporkk:sender(BotId), {pong, Line#line.body}),
			{next_state, registering, State};
		err_nicknameinuse ->
			error_logger:info_msg("Nick in use. Trying another one."),
			{next_state, registering, next_alt_nick(State)};
		reply_welcome ->
			error_logger:info_msg("Connected to IRC. Joining channels."),
			Channels = Bot#bot.channels,
			ok = gen_server:cast(sporkk:sender(BotId), {join, Channels}),
			{next_state, running, State};
		_ ->
			{next_state, registering, State}
	end.

%% ----------------------------------------------------------------------------
%% @doc Running state, in which the core handles messages from the IRC server.
%% ----------------------------------------------------------------------------
running({recv, {DateTime, LineData}}, State) ->
	BotId = State#state.botid,
	BotNick = State#state.nick,
	Bot = sporkk_cfg:get_bot(BotId),
	{ok, BareLine} = irc_lib:parse_message(Bot, DateTime, LineData),

	% Attempt to attach account information to the line.
	Line = case BareLine#line.sender of
			   none ->
				   % Line doesn't have a sender. Nothing to do.
				   BareLine;
			   User ->
				   % Check with the auth server to see if the user is logged in.
				   NewUser = sporkk_authserv:get_user(State#state.botid, User),
				   BareLine#line{sender=NewUser}
		   end,

	case Line#line.command of
		ping ->
			ok = gen_server:cast(sporkk:sender(BotId), {pong, Line#line.body}),
			{next_state, running, State};

		nickchanged ->
			{next_state, running, State#state{nick=Line#line.body}};

		privmsg ->
			case (Line#line.sender)#user.nick of
				BotNick ->
					% Ignore anything received that is from the bot.
					pass;

				_ ->
					% If the message was sent to the bot, set the destination to the sender's nick.
					Dest = case Line#line.dest of
							   BotNick -> (Line#line.sender)#user.nick;
							   Other -> Other
						   end,
					
					% Send the message to the module server for processing.
					gen_server:cast(sporkk:modserv(BotId), {event, message, {Dest, Line#line.sender, Line#line.body}}),

					% Figure out if it's a command.
					Body = Line#line.body,
					% TODO: Allow changing the command prefix.
					% TODO: Allow using the bot's nick as a command prefix.
					PfxPos = string:chr(Body, $.),
					if
						PfxPos =:= 1, length(Body) > 1 ->
							CmdMsg = string:right(Body, length(Body)-1),
							% Don't bother parsing the command. We'll let the module server handle that.
							gen_server:cast(sporkk:modserv(BotId), {command, Dest, Line#line.sender, CmdMsg});

						true ->
							pass
					end
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
set_alt_nicks(State) ->
	#bot{nicks=[Nick|AltNicks]} = sporkk_cfg:get_bot(State#state.botid),
	error_logger:info_report(Nick),
	ok = gen_server:cast(sporkk:sender(State#state.botid), {register, Nick}),
	State#state{nick=Nick, alt_nicks=AltNicks}.

next_alt_nick(State) ->
	case State#state.alt_nicks of
		[] ->
			throw(no_nick_available);
		[Nick|AltNicks] ->
			NewState = State#state{nick=Nick, alt_nicks=AltNicks},
			ok = gen_server:cast(sporkk:sender(State#state.botid), {nick, Nick}),
			NewState
	end.


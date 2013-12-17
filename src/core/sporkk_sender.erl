%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Server process that handles sending messages to the IRC server.
%% ============================================================================
-module(sporkk_sender).
-behavior(gen_server).
-include("sporkk.hrl").

%% API Functions
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(state, {connector}).


%% ============================================================================
%% API Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the bot manager.
%% ----------------------------------------------------------------------------
start_link(Bot, ConnectorId) ->
	gen_server:start_link(Bot#bot.sender, ?MODULE, [ConnectorId], []).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Initializes the server's state.
%% ----------------------------------------------------------------------------
init([ConnectorId]) ->
	{ok, #state{connector=ConnectorId}}.

%% ----------------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State}          |
%%                                            {reply, Reply, State, Timeout} |
%%                                            {noreply, State}               |
%%                                            {noreply, State, Timeout}      |
%%                                            {stop, Reason, Reply, State}   |
%%                                            {stop, Reason, State}
%% @doc Handles call messages.
%% ----------------------------------------------------------------------------
handle_call(_Request, _From, State) ->
	{ok, State}.

%% ----------------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State}             |
%%                                  {noreply, State, Timeout}    |
%%                                  {stop, Reason, State}
%% @doc Handles cast messages.
%% ----------------------------------------------------------------------------
handle_cast({pong, Server}, State) ->
	ok = gen_server:cast(State#state.connector, {send_line, irc_lib:pong(Server)}),
	{noreply, State};

handle_cast({register, Nick}, State) ->
	error_logger:info_msg("Registering with server as ~s~n", [Nick]),
	ok = gen_server:cast(State#state.connector, {send_line, irc_lib:register(Nick)}),
	ok = gen_server:cast(State#state.connector, {send_line, irc_lib:register(Nick, "localhost", "localhost", "Sporkk")}),
	{noreply, State};

handle_cast({nick, Nick}, State) ->
	ok = gen_server:cast(State#state.connector, {send_line, irc_lib:register(Nick)}),
	{noreply, State};

handle_cast({whois, Nick}, State) ->
	ok = gen_server:cast(State#state.connector, {send_line, irc_lib:whois(Nick)}),
	{noreply, State};

handle_cast({privmsg, Dest, Message}, State) ->
	ok = gen_server:cast(State#state.connector, {send_line, irc_lib:privmsg(Dest, Message)}),
	{noreply, State};

handle_cast({kick, Dest, Nick, Reason}, State) ->
	ok = gen_server:cast(State#state.connector, {send_line, irc_lib:kick(Dest, Nick, Reason)}),
	{noreply, State};

handle_cast({join, Channels}, State) ->
	error_logger:info_msg("Joining channels ~s~n", [string:join(Channels, ", ")]),
	ok = gen_server:cast(State#state.connector, {send_line, irc_lib:join(Channels)}),
	{noreply, State};

% Ignore things that don't speak the same language. Mainly because I'm American.
handle_cast(_Request, State) ->
	{noreply, State}.


%% ----------------------------------------------------------------------------
%% @doc Handle non cast/call messages.
%% ----------------------------------------------------------------------------
handle_info(_Request, State) ->
	{noreply, State}.


%% ----------------------------------------------------------------------------
%% @doc Called when the server terminates.
%% ----------------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%% ----------------------------------------------------------------------------
%% @doc Called when code changes.
%% ----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ============================================================================
%% Internal Functions
%% ============================================================================


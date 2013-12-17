%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Server process that manages the bot's connection to the IRC server.
%% ============================================================================
-module(sporkk_connector).
-behavior(gen_server).
-include("sporkk.hrl").

%% API Functions
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(state, {bot, sock, receiver}).


%% ============================================================================
%% API Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the bot manager.
%% ----------------------------------------------------------------------------
start_link(Bot, Id, ReceiverId) ->
	gen_server:start_link(Id, ?MODULE, [Bot, ReceiverId], []).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Initializes the server's state.
%% ----------------------------------------------------------------------------
init([Bot, ReceiverId]) ->
	{ok, Network} = sporkk_cfg:get_network(Bot#bot.network),
	error_logger:info_msg("Connecting to IRC network."),
	{ok, Sock} = network_connect(Network),
	% Notify the receiver that we've connected.
	gen_fsm:send_event(ReceiverId, connected),
	{ok, #state{bot=Bot, sock=Sock, receiver=ReceiverId}}.

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
handle_cast({send_line, Data}, State) ->
	gen_tcp:send(State#state.sock, Data ++ "\r\n"),
	{noreply, State};

% Ignore things that don't speak the same language. Mainly because I'm American.
handle_cast(_Request, State) ->
	{noreply, State}.


%% ----------------------------------------------------------------------------
%% @doc Handle non cast/call messages.
%% ----------------------------------------------------------------------------
%% @doc Handles TCP messages, parsing them into lines and dispatching the lines to the core for processing.
handle_info({tcp, Sock, Data}, State) ->
	Lines = string:tokens(Data, "\r\n"),
	dispatch(State#state.receiver, Lines),
	{noreply, State#state{sock=Sock}};

handle_info({tcp_closed, _Sock}, State) ->
	% TODO: Handle the TCP connection closing.
	error_logger:info_msg("TCP connection closed."),
	{noreply, State};

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

%% @doc Attempts to connect to the given IRC network and returns {ok, Servers, Sock}.
network_connect({_Id, Servers}) ->
	connect(Servers).

connect([]) ->
	{error, connection_failed};
connect([{Addr, Port} | Servers]) ->
	error_logger:info_msg("Attempting to connect to server ~s:~p.~n", [Addr, Port]),
	case gen_tcp:connect(Addr, Port, [{packet, line}, {active, true}]) of
		{ok, Sock} ->
			{ok, Sock};
		_ ->
			connect(Servers)
	end.

%% @doc Dispatches lines to the core for processing.
dispatch(_Receiver, []) ->
	ok;
dispatch(Receiver, [Line|Lines]) ->
	gen_fsm:send_event(Receiver, {recv, {calendar:universal_time(), Line}}),
	dispatch(Receiver, Lines).


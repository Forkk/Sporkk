%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Server process that manages the bot's modules.
%% ============================================================================
-module(sporkk_modserv).
-behavior(gen_server).
-include("sporkk.hrl").

%% API Functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(state, {botid, modules, commands}).


%% ============================================================================
%% API Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the bot manager.
%% ----------------------------------------------------------------------------
start_link(BotId) ->
	gen_server:start_link(sporkk:modserv(BotId), ?MODULE, [BotId], []).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Initializes the server's state.
%% ----------------------------------------------------------------------------
init([BotId]) ->
	% Send messages to self to load all the modules that the bot has enabled.
	{ok, #state{botid=BotId, modules=[], commands=[]}}.

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
handle_cast({load_mod, Mod}, State) ->
	case sporkk_modsup:start_mod(State#state.botid, Mod) of
		{ok, Pid} ->
			NewModList = lists:append(State#state.modules, [{Mod, Pid}]),
			{noreply, State#state{modules=NewModList}};
		{ok, Pid, _Info} ->
			NewModList = lists:append(State#state.modules, [{Mod, Pid}]),
			{noreply, State#state{modules=NewModList}};

		{error, {already_started, _Pid}} ->
			error_logger:warning_msg("Tried to load already started bot module ~w~n", [Mod]),
			{noreply, State};

		{error, Error} ->
			error_logger:error_msg("Failed to load bot module ~w: ~w~n", [Mod, Error]),
			{noreply, State}
	end;

% Handle event messages.
handle_cast({event, {EventType, EventData}}, State) ->
	% Forward the event to every loaded module.
	lists:foreach(fun({_Mod, Pid}) ->
						  gen_server:cast(Pid, {event, {EventType, EventData}})
				  end, State#state.modules),
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


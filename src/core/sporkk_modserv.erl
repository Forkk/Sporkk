%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Server process that manages the bot's modules.
%% ============================================================================
-module(sporkk_modserv).
-behavior(gen_server).
-include("sporkk.hrl").
-include("modules.hrl").

%% API Functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(state, {botid, modules, commands}).

% Record for storing module info.
-record(module, {id, pid, name, desc, short_desc, version}).

% Record for storing command info.
-record(command, {id, name, desc, args, module}).

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
			{ok, NewState} = register_mod(Mod, Pid, State),
			{noreply, NewState};
		{ok, Pid, _Info} ->
			{ok, NewState} = register_mod(Mod, Pid, State),
			{noreply, NewState};

		{error, {already_started, _Pid}} ->
			error_logger:warning_msg("Tried to load already started bot module ~w~n", [Mod]),
			{noreply, State};

		{error, Error} ->
			error_logger:error_msg("Failed to load bot module ~w: ~w~n", [Mod, Error]),
			{noreply, State}
	end;

% Handle event messages.
handle_cast({event, EventType, EventData}, State) ->
	% Forward the event to every loaded module.
	lists:foreach(fun(Module) ->
						  gen_server:cast(Module#module.pid, {event, EventType, EventData})
				  end, State#state.modules),
	{noreply, State};

% Handle command messages.
handle_cast({command, Source, User, CmdMsg}, State) ->
	% Parse the command.
	% TODO: Allow quoting and escaping spaces in command strings. For now, we'll just tokenize spaces.
	[Command | Args] = string:tokens(CmdMsg, " "),

	% Find a command spec matching the command.
	case lists:filter(fun(E) -> E#command.name == Command end, State#state.commands) of
		[CmdData | _] ->
			% Find the command's module.
			% This is a bit of a hack that relies on the 'id' field in the record being the second entry in the tuple.
			Module = lists:keyfind(CmdData#command.module, 2, State#state.modules),
			% Send the command message to the module.
			gen_server:cast(Module#module.pid, {command, CmdData#command.id, Source, User, Args}),
			ok;

		[] ->
			% TODO: Add "creative" command not found messages.
			sporkk:send(State#state.botid, Source, "Command not found."),
			ok
	end,
	{noreply, State};

% Dynamic command registry (not fully supported yet).
handle_cast({register_cmd, Mod, CmdInfo}, State) ->
	{noreply, register_cmds([{Mod, CmdInfo}], State)};

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

%% @doc Registers the given module and returns a new value for the modserv's state.
register_mod(Mod, Pid, State) ->
	ModInfo = Mod:get_info(),
	Module = #module{
				id = Mod,
				pid = Pid,
				name = ModInfo#mod_info.name,
				desc = ModInfo#mod_info.desc,
				short_desc = ModInfo#mod_info.short_desc,
				version = ModInfo#mod_info.version
			   },
	NewModList = lists:append(State#state.modules, [Module]),

	{ok, NewState} = register_cmds(lists:map(fun(E) -> {Mod, E} end, ModInfo#mod_info.commands), State),
	{ok, NewState#state{modules=NewModList}}.

%% @doc Registers the given command info for the given module into the given state and returns the new state.
register_cmds([], State) ->
	{ok, State};
register_cmds([{Mod, CmdInfo}|More], State) ->
	Command = #command{
				 id = CmdInfo#cmd_info.id,
				 name = CmdInfo#cmd_info.name,
				 desc = CmdInfo#cmd_info.desc,
				 args = CmdInfo#cmd_info.args,
				 module = Mod
				},
	error_logger:info_msg("~w~n", [lists:append(State#state.commands, [Command])]),
	register_cmds(More, State#state{commands=lists:append(State#state.commands, [Command])}).


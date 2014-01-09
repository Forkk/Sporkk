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
-export([start_link/1, modules/1, commands/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
% botid is the bot's ID.
% modules is a list of {ModId, MonitorId} tuples, where ModId is the module's 
% Erlang module name, and MonitorId is the module's process monitor ref.
-record(state, {botid, modules}).

%% ============================================================================
%% API Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the bot manager.
%% ----------------------------------------------------------------------------
start_link(BotId) ->
	gen_server:start_link(sporkk:modserv(BotId), ?MODULE, [BotId], []).


%% @doc Gets a list of the modserv's registered modules.
modules(BotId) ->
	gen_server:call(sporkk:modserv(BotId), get_module_list).

%% @doc Gets a list of the modserv's available commands.
commands(BotId) ->
	gen_server:call(sporkk:modserv(BotId), get_command_list).

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Initializes the server's state.
%% ----------------------------------------------------------------------------
init([BotId]) ->
	% Send messages to self to load all the modules that the bot has enabled.
	{ok, #state{botid=BotId, modules=[]}}.

%% ----------------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State}          |
%%                                            {reply, Reply, State, Timeout} |
%%                                            {noreply, State}               |
%%                                            {noreply, State, Timeout}      |
%%                                            {stop, Reason, Reply, State}   |
%%                                            {stop, Reason, State}
%% @doc Handles call messages.
%% ----------------------------------------------------------------------------
handle_call(get_module_list, _From, State) ->
	{reply, lists:map(fun({Mod, _Mon}) -> Mod:get_info() end, State#state.modules), State};

handle_call(get_command_list, _From, State) ->
	% Combine the command list for each module.
	ModCmdList = lists:concat(lists:map(fun({Mod, _Mon}) -> (Mod:get_info())#mod_info.commands end, State#state.modules)),
	{reply, ModCmdList, State};

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

handle_cast({unload_mod, Mod}, State) ->
	case lists:keyfind(Mod, 1, State#state.modules) of
		{Mod, _Pid} ->
			% Remove the module from the module list.
			NewModList = lists:filter(fun({M, _Mon}) -> M =/= Mod end, State#state.modules),
			% Stop the module's process.
			ok = sporkk_modsup:stop_mod(State#state.botid, Mod),
			{noreply, State#state{modules=NewModList}};
		false ->
			{noreply, State}
	end;

% Sent from a module process when it starts.
handle_cast({mod_start, Mod}, State) ->
	MonRef = monitor(process, global:whereis_name(mod_proc(State#state.botid, Mod))),
	NewModList = lists:keystore(Mod, 1, State#state.modules, {Mod, MonRef}),
	{noreply, State#state{modules=NewModList}};

% Handle event messages.
handle_cast({event, EventType, EventData}, State) ->
	% Forward the event to every loaded module.
	lists:foreach(fun({Mod, _Mon}) ->
						  gen_server:cast(mod_proc(State#state.botid, Mod), {event, EventType, EventData})
				  end, State#state.modules),
	{noreply, State};

% Handle command messages.
handle_cast({command, Source, User, CmdMsg}, State) ->
	% Parse the command.
	% TODO: Allow quoting and escaping spaces in command strings. For now, we'll just tokenize spaces.
	[CommandName | Args] = string:tokens(CmdMsg, " "),

	% Find a matching command.
	case find_cmd(CommandName, State) of
		{ok, Command, {Mod, _Mon}} ->
			% Send the command message to the module.
			gen_server:cast(mod_proc(State#state.botid, Mod), {command, Command#cmd_info.id, Source, User, Args}),
			{noreply, State};

		not_found ->
			% TODO: Add "creative" command not found messages.
			sporkk:send(State#state.botid, Source, "Command not found."),
			{noreply, State}
	end;

% Ignore things that don't speak the same language. Mainly because I'm American.
handle_cast(_Request, State) ->
	{noreply, State}.


%% ----------------------------------------------------------------------------
%% @doc Handle non cast/call messages.
%% ----------------------------------------------------------------------------
% Handle module processes crashing.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
	{Mod, Ref} = lists:keyfind(Ref, 2, State#state.modules),
	ErrMsg = io_lib:format("Sporkk module ~w on bot ~w has crashed.", [Mod, State#state.botid]),
	error_logger:error_msg(ErrMsg, []),
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

%% @spec find_cmd(Name, State) -> {ok, Command, Module} | not_found
%% @doc Finds the command with the given Name.
find_cmd(Name, State) ->
	% To find a command, we need to search through the module list and look for it in each module.
	% To do this, we'll use lists:filtermap on the module list and return a list of commands that match.
	case lists:concat(lists:filtermap(fun({Mod, Pid}) -> find_cmd_in_mod(Mod, Pid, Name) end, State#state.modules)) of
		[] ->
			not_found;
		[{Command, Module} | _OtherMods] ->
			{ok, Command, Module}
	end.

% filtermap predicate for finding the given command in the given module.
find_cmd_in_mod(Mod, Pid, CmdName) ->
	% Get the module's info.
	ModInfo = Mod:get_info(),
	% Filter the list of commands and put each in a tuple with its module name.
	case lists:filtermap(fun(Cmd) -> 
								 if Cmd#cmd_info.name =:= CmdName ->
										{true, {Cmd, {Mod, Pid}}};
									true ->
										false
								 end
						 end, ModInfo#mod_info.commands) of
		[] ->
			false;
		Cmds ->
			{true, Cmds}
	end.


%% @doc Registers the given module and returns a new value for the modserv's state.
register_mod(Mod, Id, State) ->
	{ok, State#state{modules=lists:append(State#state.modules, [{Mod, Id}])}}.

%% @doc Gets the process registry ID that the given module should be registered as.
mod_proc(BotId, Mod) ->
	{global, {BotId, module, Mod}}.


%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Supervisor for all of a bot's module processes.
%% ============================================================================
-module(sporkk_modsup).
-author("Forkk").
-include("sporkk.hrl").

-behavior(supervisor).

% API
-export([start_link/1, start_mod/2]).

% Callbacks
-export([init/1]).

%% ============================================================================
%% API Functions
%% ============================================================================

%% @doc Starts a bot module process with the given module name on the given bot ID.
start_mod(BotId, ModName) ->
	supervisor:start_child(proc(BotId),
						   {ModName, 
							{sporkk_module, start_link, [BotId, ModName]},
							permanent,
							2000,
							worker,
							[sporkk_module, ModName]
						   }).

%% ----------------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Starts the supervisor.
%% ----------------------------------------------------------------------------
start_link(BotId) ->
	supervisor:start_link(proc(BotId), ?MODULE, []).


%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Returns information about the supervisor.
%% ----------------------------------------------------------------------------
init([]) ->
	{ok, {{one_for_one, 5, 60}, []}}.


%% ============================================================================
%% Internal Functions
%% ============================================================================
proc(BotId) ->
	{global, list_to_atom(atom_to_list(BotId) ++ "_" ++ atom_to_list(modsup))}.


%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Top level supervisor for Sporkk.
%% ============================================================================

-module(sporkk_sup).
-author("Forkk").
-include("sporkk.hrl").

-behavior(supervisor).

% API
-export([start_link/0, gen_spec/1]).

% Callbacks
-export([init/1]).

%% ============================================================================
%% API Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Starts the supervisor.
%% ----------------------------------------------------------------------------
start_link() ->
	supervisor:start_link({global, ?MODULE}, ?MODULE, []).


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
	{ok, {{one_for_all, 5, 60}, []}}.

%% ----------------------------------------------------------------------------
%% @spec gen_spec(Bot) -> ChildSpec
%% @doc Returns a ChildSpec tuple for the given #bot record.
%% ----------------------------------------------------------------------------
gen_spec(BotId) ->
	% Ensure the bot ID exists to prevent starting nonexistant bots.
	sporkk_cfg:get_bot_entry(BotId),
	{BotId,
	 {sporkk_bot_sup, start_link, [BotId]},
	 permanent,
	 8000,
	 supervisor,
	 [sporkk_bot_sup]}.


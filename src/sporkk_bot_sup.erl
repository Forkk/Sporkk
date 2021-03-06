%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Supervisor for a single bot.
%% ============================================================================

-module(sporkk_bot_sup).
-author("Forkk").
-include("sporkk.hrl").

-behavior(supervisor).

% API
-export([start_link/1]).

% Callbacks
-export([init/1]).

%% ============================================================================
%% API Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @doc Starts the supervisor.
%% ----------------------------------------------------------------------------
start_link(Bot) ->
	supervisor:start_link({global, ?MODULE}, ?MODULE, [Bot]).


%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Returns information about the supervisor.
%% ----------------------------------------------------------------------------
init([BotId]) ->
	{ok,
	 {{one_for_all, 5, 60},
	  [
	   % The core process. This is responsible for keeping track of state and config information.
	   {sporkk_core,
		{sporkk_core, start_link, [BotId]},
		permanent,
		5000,
		worker,
		[sporkk_core]
	   },
	   % The sender process. This is responsible for sending messages to the IRC server.
	   {sporkk_sender,
		{sporkk_sender, start_link, [BotId]},
		permanent,
		5000,
		worker,
		[sporkk_sender]
	   },
	   % The receiver process. This processes data from the connector into line records and hands them to the router.
	   {sporkk_receiver,
		{sporkk_receiver, start_link, [BotId]},
		permanent,
		5000,
		worker,
		[sporkk_receiver]
	   },
	   % The connector process. This manages the bot's connection to the IRC server.
	   {sporkk_connector,
		{sporkk_connector, start_link, [BotId]},
		permanent,
		5000,
		worker,
		[sporkk_connector]
	   },
	   % The supervisor for individual module processes.
	   {sporkk_modsup,
		{sporkk_modsup, start_link, [BotId]},
		permanent,
		5000,
		worker,
		[sporkk_modsup]
	   },
	   % The module server. This manages loading and interacting with the bot's modules.
	   {sporkk_modserv,
		{sporkk_modserv, start_link, [BotId]},
		permanent,
		5000,
		supervisor,
		[sporkk_modserv]
	   },
	   % The authentication server for managing user sessions and tracking who's logged in as who.
	   {sporkk_authserv,
		{sporkk_authserv, start_link, [BotId]},
		permanent,
		5000,
		worker,
		[sporkk_authserv]
	   }
	  ]}}.


%% ============================================================================
%% Internal Functions
%% ============================================================================


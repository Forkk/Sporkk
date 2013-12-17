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
init([BotSpec]) ->
	ConnectorId = {global, sporkk_connector},
	SenderId = {global, sporkk_sender},
	RouterId = {global, sporkk_router},
	ReceiverId = {global, sporkk_receiver},
	Bot = BotSpec#bot{sender=SenderId, router=RouterId},
	{ok,
	 {{one_for_all, 5, 60},
	  [
	   % The sender process. This is responsible for sending messages to the IRC server.
	   {sporkk_sender,
		{sporkk_sender, start_link, [Bot, ConnectorId]},
		permanent,
		2000,
		worker,
		[sporkk_sender]
	   },
	   % The receiver process. This processes data from the connector into line records and hands them to the router.
	   {sporkk_receiver,
		{sporkk_receiver, start_link, [Bot, ReceiverId]},
		permanent,
		2000,
		worker,
		[sporkk_receiver]
	   },
	   % The connector process. This manages the bot's connection to the IRC server.
	   {sporkk_connector,
		{sporkk_connector, start_link, [Bot, ConnectorId, ReceiverId]},
		permanent,
		2000,
		worker,
		[sporkk_connector]
	   }
%	   % The event manager.
%	   {event_mgr,
%		{gen_event, start_link, []},
%		permanent,
%		2000,
%		worker,
%		[gen_event]
%	   }
	  ]}}.


%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Module with API functions for Sporkk's configs.
%% ============================================================================
-module(sporkk_cfg).
-include("sporkk.hrl").

-include_lib("stdlib/include/qlc.hrl").

% API Functions
-export([init/0]).
-export([add_bot/4, remove_bot/1, get_bots/0]).
-export([add_network/2, remove_network/1, get_network/1]).

% Records

% Record for storing information about a bot's configuration.
-record(bot_config, {
		  % An ID that uniquely identifies this bot.
		  id,
		  % The ID of the network that this bot connects to.
		  network,
		  % The bot's nick.
		  nick,
		  % List of strings representing the channels this bot will connect to.
		  channels=[],
		  % If the bot is enabled, 'true', else 'false'.
		  enabled=true
		 }).

% Record for storing information about an IRC network.
-record(net_config, {
		  % An ID that uniquely identifies this network.
		  id,
		  servers=[]
		 }).

%% ============================================================================
%% API Functions
%% ============================================================================

%% @doc Initializes Sporkk's schema and tables in Mnesia.
%% @spec init() -> ok
init() ->
	mnesia:create_schema([node()]),
	mnesia:create_table(bot_config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, bot_config)}]),
	mnesia:create_table(net_config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, net_config)}]),
	ok = mnesia:wait_for_tables([bot_config, net_config], 5000),
	ok.


%%%%%% Bots %%%%%%

%% @doc Adds a bot to the config. This is called by the bot manager's add_bot handler to add bots to the database.
%% @spec add_bot(Id, Nick, NetworkId, Channels) -> {ok, Bot#bot{}}
add_bot(Id, Nick, NetworkId, Channels) ->
	BotConf = #bot_config{id=Id, nick=Nick, network=NetworkId, channels=Channels, enabled=true},
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(BotConf) end),
	{ok, bot_from_config(BotConf)}.

%% @doc Removes the bot with the given ID from the database. Replies 'ok' on success.
%% @spec remove_bot(Id) -> ok | Error
remove_bot(Id) ->
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:delete(bot_config, Id, write) end),
	ok.

%% @doc Gets a list of 'bot' records containing information about all the bots in the database.
%% @spec get_bots() ->  {ok, Bots} | {error, Error}
get_bots() ->
	Query = qlc:q([C || C <- mnesia:table(bot_config), C#bot_config.enabled =:= true]),
	{atomic, BotConfs} = mnesia:transaction(fun() -> qlc:e(Query) end),
	Bots = lists:map(fun(C) -> bot_from_config(C) end, BotConfs),
	{ok, Bots}.


%%%%%% Networks %%%%%%

%% @doc Adds a new IRC network with the given ID and server list, returning {ok, Network} if successful.
%%      The server list should be a list of tuples in the format {Address, Port}.
add_network(Id, Servers) ->
	% Verify the server list is valid before we put it in the database.
	Servers = [{Addr, Port} || {Addr, Port} <- Servers],
	{atomic, ok} = mnesia:transaction(fun() -> mnesia:write(#net_config{id=Id, servers=Servers}) end),
	{ok, {Id, Servers}}.

%% @doc Removes the IRC network with the given ID.
remove_network(Id) ->
	mnesia:delete(net_config, Id).

%% @doc Gets the IRC network with the given ID.
get_network(Id) ->
	Query = qlc:q([C || C <- mnesia:table(net_config), C#net_config.id =:= Id]),
	{atomic, [#net_config{id=Id, servers=Servers}|_]} = mnesia:transaction(fun() -> qlc:e(Query) end),
	{ok, {Id, Servers}}.


%% ============================================================================
%% Internal Functions
%% ============================================================================
bot_from_config(BotConf) ->
	#bot{
	   id=BotConf#bot_config.id,
	   network=BotConf#bot_config.network,
	   nick=BotConf#bot_config.nick,
	   channels=BotConf#bot_config.channels
	  }.


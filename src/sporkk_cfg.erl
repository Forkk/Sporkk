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
-export([add_bot/4, remove_bot/1, get_bots/0, get_bot/1, get_bot_extra/2, get_bot_extra/3, set_bot_extra/3, add_bot_chans/2, remove_bot_chans/2, add_bot_mod/2, remove_bot_mod/2]).
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
		  % Set of strings representing the channels this bot will connect to.
		  channels=sets:new(),
		  % Set of atoms representing the modules the bot should load on startup.
		  modules=sets:new(),
		  % List of extra configuration options.
		  extra=[],
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

%% @doc Initializes Sporkk's tables in Mnesia.
%% @spec init() -> ok
init() ->
	mnesia:create_table(bot_config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, bot_config)}]),
	mnesia:create_table(net_config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, net_config)}]),
	ok = mnesia:wait_for_tables([bot_config, net_config], 5000),
	ok.


%%%%%% Bots %%%%%%

%% @doc Adds a bot to the config. This is called by the bot manager's add_bot handler to add bots to the database.
%% @spec add_bot(Id, Nick, NetworkId, Channels) -> {ok, Bot#bot{}}
add_bot(Id, Nick, NetworkId, Channels) ->
	BotConf = #bot_config{id=Id, nick=Nick, network=NetworkId, channels=sets:from_list(Channels), enabled=true},
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

%% @doc Gets the bot with the given ID.
get_bot(Id) ->
	Query = qlc:q([C || C <- mnesia:table(bot_config), C#bot_config.id =:= Id]),
	{atomic, [BotConf|_]} = mnesia:transaction(fun() -> qlc:e(Query) end),
	{ok, bot_from_config(BotConf)}.

%% @doc Adds the given channels to the bot with the given ID.
%% 		NOTE: This does *NOT* tell the bot's process to join the channels. It only changes the DB entry.
%% 		To make a bot join channels right now, call sporkk:join(BotId, Channels).
add_bot_chans(BotId, Channels) ->
	{atomic, ok} = mnesia:transaction(
					 fun() ->
							 [Bot] = mnesia:wread({bot_config, BotId}),
							 % Combine the channels set with the Channels list.
							 NewChans = sets:union(Bot#bot_config.channels, sets:from_list(Channels)),
							 mnesia:write(bot_config, Bot#bot_config{channels=NewChans}, write)
					 end),
	ok.

%% @doc Removes the given channels from the bot with the given ID.
%% 		NOTE: This does *NOT* tell the bot's process to part from the channels. It only changes the DB entry.
%% 		To make a bot part from channels right now, call sporkk:part(BotId, Channels).
remove_bot_chans(BotId, Channels) ->
	{atomic, ok} = mnesia:transaction(
					 fun() ->
							 [Bot] = mnesia:wread({bot_config, BotId}),
							 % Remove everything in the Channels list from the channels set.
							 NewChans = sets:subtract(Bot#bot_config.channels, sets:from_list(Channels)),
							 mnesia:write(bot_config, Bot#bot_config{channels=NewChans}, write)
					 end),
	ok.

%% @doc Adds the given module to the bot with the given ID.
%% 		NOTE: This does *NOT* tell the bot's process to load the module. It only changes the DB entry.
add_bot_mod(BotId, Module) ->
	{atomic, ok} = mnesia:transaction(
					 fun() ->
							 [Bot] = mnesia:wread({bot_config, BotId}),
							 NewMods = sets:add_element(Module, Bot#bot_config.modules),
							 mnesia:write(bot_config, Bot#bot_config{modules=NewMods}, write)
					 end),
	ok.

%% @doc Removes the given module from the bot with the given ID.
%% 		NOTE: This does *NOT* tell the bot's process to unload the module. It only changes the DB entry.
remove_bot_mod(BotId, Module) ->
	{atomic, ok} = mnesia:transaction(
					 fun() ->
							 [Bot] = mnesia:wread({bot_config, BotId}),
							 NewMods = sets:del_element(Module, Bot#bot_config.modules),
							 mnesia:write(bot_config, Bot#bot_config{modules=NewMods}, write)
					 end),
	ok.

%% @doc Gets the extra config option with the given key from the given bot ID's configuration.
get_bot_extra(BotId, Key) ->
	case get_bot(BotId) of
		{ok, #bot{extras=Extras}} ->
			case lists:keyfind(Key, 1, Extras) of
				{Key, Value} ->
					{ok, Value};
				false ->
					no_key
			end;
		no_bot ->
			no_bot
	end.
get_bot_extra(BotId, Key, Default) ->
	case get_bot_extra(BotId, Key) of
		no_key ->
			Default;
		Value ->
			Value
	end.

set_bot_extra(BotId, Key, Value) ->
	{atomic, ok} = mnesia:transaction(
					 fun() ->
							 [Bot] = mnesia:wread({bot_config, BotId}),
							 % Combine the channels set with the Channels list.
							 NewExtras = lists:keystore(Key, 1, Bot#bot_config.extra, {Key, Value}),
							 mnesia:write(bot_config, Bot#bot_config{extra=NewExtras}, write)
					 end),
	ok.


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
	   channels=sets:to_list(BotConf#bot_config.channels),
	   modules=sets:to_list(BotConf#bot_config.modules),
	   extras=BotConf#bot_config.extra
	  }.


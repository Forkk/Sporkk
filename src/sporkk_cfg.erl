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

-export([
		 add_bot/4, remove_bot/1, get_bots/0, get_bot/1,
		 get_bot_extra/2, get_bot_extra/3, set_bot_extra/3,
		 add_bot_chans/2, remove_bot_chans/2,
		 add_bot_mod/2, remove_bot_mod/2
		]).

-export([
		 get_user/3,
		 create_user/2, add_user/2, remove_user/1,
		 add_user_group/3, remove_user_group/3
		]).

-export([
		 add_network/2, remove_network/1, get_network/1
		]).

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
		  % List of {User, GroupId} tuples, mapping users' nickserv accounts to their user group for this bot.
		  usergrps=[],
		  % List of {Module, [GroupId|_]} or {module, all} tuples for specifying which users can use which modules.
		  modgrps=[],
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

% Record for storing user information.
-record(usr_config, {
		  % The user's username.
		  name,
		  % The user's hashed, salted password.
		  pass,
		  % The user's global groups - groups the user is in for every bot.
		  % Primarily used for "owners" of the bot.
		  groups=sets:new(),
		  % Extra information.
		  extras=[]
		 }).

%% ============================================================================
%% API Functions
%% ============================================================================

%% @doc Initializes Sporkk's tables in Mnesia.
%% @spec init() -> ok
init() ->
	mnesia:create_table(bot_config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, bot_config)}]),
	mnesia:create_table(net_config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, net_config)}]),
	mnesia:create_table(usr_config, [{type, set}, {disc_copies, [node()]}, {attributes, record_info(fields, usr_config)}]),
	ok = mnesia:wait_for_tables([bot_config, net_config, usr_config], 5000),
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
			{ok, Default};
		{ok, Value} ->
			{ok, Value}
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


%%%%%% User %%%%%%

%% @doc Creates a new user with the given password. The password will be hashed using the passwd module.
create_user(Name, Pass) ->
	add_user(Name, passwd:hash_pass(sha512, Pass)).

%% @doc Adds a user to the database.
add_user(Name, PassHash) ->
	User = #usr_config{name=Name, pass=PassHash, groups=sets:new()},
	case mnesia:transaction(fun() ->
									case mnesia:wread({usr_config, Name}) of
										[] ->
											% User doesn't exist. Write the new user to the database.
											mnesia:write(User);
										[_User] ->
											% The user exists. Return an error.
											user_exists
									end
							end) of
		{atomic, ok} ->
			ok;
		{atomic, Error} ->
			{error, Error}
	end.

%% @doc Adds a user to the database.
remove_user(Name) ->
	case mnesia:transaction(fun() ->
									case mnesia:wread({usr_config, Name}) of
										[] ->
											% User doesn't exist. Error.
											no_user;
										[_User] ->
											% Go through all the bots and remove this user from groups.
											lists:map(fun(Bot) ->
															  % Filter out group entries with the user's name in them.
															  mnesia:write(Bot#bot_config{usergrps=lists:filter(fun({EntryName, _Group}) -> EntryName =/= Name end)})
													  end, mnesia:wread(bot_config)),
											ok
									end
							end)
	of
		{atomic, ok} ->
			ok;
		{atomic, Error} ->
			{error, Error}
	end.

%% @doc Fills out the given 'user' record with the given username's account info.
%% Returns {ok, User}, where User is a 'user' record, if successful.
get_user(BotId, Name, User) ->
	case mnesia:transaction(
		   fun() ->
				   [Bot] = mnesia:read({bot_config, BotId}),
				   case mnesia:read({usr_config, Name}) of
					   [UsrCfg] ->
						   {Bot, UsrCfg};
					   [] ->
						   no_user
				   end
		   end)
	of
		{atomic, {Bot, UserCfg}} ->
			% Get the user's groups for this bot.
			BotGroups = case lists:keyfind(Name, 1, Bot#bot_config.usergrps) of
							false -> sets:new();
							BotGrps -> BotGrps 
						end,
			% Merge the user's bot group list with the user's global group list, set the username, and return.
			{ok, User#user{
				   username=Name,
				   groups=sets:union(UserCfg#usr_config.groups, BotGroups)
				  }};
		{atomic, Error} ->
			{error, Error}
	end.


%% @doc Adds the user with the given username to the given group ID on the given bot.
add_user_group(BotId, UserName, GroupId) ->
	mod_user_group(add, BotId, UserName, GroupId).

%% @doc Removes the user with the given username from the given group ID on the given bot.
remove_user_group(BotId, UserName, GroupId) ->
	mod_user_group(del, BotId, UserName, GroupId).

mod_user_group(Type, BotId, UserName, GroupId) ->
	case
		mnesia:transaction(
		  fun() ->
				  [Bot] = mnesia:wread({bot_config, BotId}),
				  case mnesia:wread({usr_config, UserName}) of
					  [_User] ->
						  OldSet = case lists:keyfind(UserName, 1, Bot#bot_config.usergrps) of
									   {UserName, Groups} -> 
										   Groups;
									   false ->
										   sets:new()
								   end,
						  NewEntry = case Type of
										 add ->
											 sets:add_element(GroupId, OldSet);
										 del ->
											 sets:del_element(GroupId, OldSet)
									 end,
						  NewUserGrps = lists:keystore(UserName, 1, Bot#bot_config.usergrps, NewEntry),
						  mnesia:write(bot_config, Bot#bot_config{usergrps=NewUserGrps});
					  [] ->
						  no_user
				  end
		  end)
	of
		{atomic, ok} ->
			ok;
		{atomic, Error} ->
			{error, Error}
	end.


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


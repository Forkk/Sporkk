%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Module with API functions for Sporkk's configs.
%% ============================================================================
-module(sporkk_cfg).
-include("db.hrl").
-include("sporkk.hrl").
-include("modules.hrl").

-include_lib("stdlib/include/qlc.hrl").

% API Functions
-export([
		 get_config_section/1, get_config_section/2
		]).

-export([get_bot_entries/0, get_bot_entry/1]).

-export([
		 get_user/3,
		 create_user/2, add_user/2, remove_user/1
		]).

%% ============================================================================
%% API Functions
%% ============================================================================

%% @doc Gets the config section with the given key.
get_config_section(Key) ->
	{ok, Entries} = file:consult("sporkk.cfg"),
	case lists:keyfind(Key, 1, Entries) of
		false ->
			throw(no_section);
		{Key, Value} ->
			Value
	end.

%% @doc Gets the config section with the given key. Returns Default if it doesn't exist.
get_config_section(Key, Default) ->
	try get_config_section(Key) of
		Value ->
			Value
	catch
		no_section ->
			Default
	end.

%%%%%% Bots %%%%%%

get_bot_entries() ->
	get_config_section(bots).

%% @doc Gets the bot with the given ID.
get_bot_entry(Id) ->
	case lists:keyfind(Id, 1, get_bot_entries()) of
		false ->
			throw(no_bot);
		Bot ->
			Bot
	end.


%%%%%% User %%%%%%

%% @doc Creates a new user with the given password. The password will be hashed using sha512 and then passwed to the the passwd module to be salted and hashed again.
%% Passwords are hashed twice so that when a user wants to authenticate, the password can be hashed *before* being sent to the auth server.
%% This way, if an auth request is sent to the auth server and causes the auth server to crash, when the Erlang shell prints the last message sent to the server, the password won't be in plain text.
create_user(Name, Pass) ->
	add_user(Name, passwd:hash_pass(sha512, crypto:hash(sha512, Pass))).

%% @doc Adds a user to the database.
add_user(Name, PassHash) ->
	User = #usr_config{name=Name, pass=PassHash},
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
											mnesia:delete({usr_config, Name}),
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
				   case mnesia:read({usr_config, Name}) of
					   [UsrCfg] ->
						   {ok, UsrCfg};
					   [] ->
						   no_user
				   end
		   end)
	of
		{atomic, {ok, UserCfg}} ->
			% Get the user's groups for this bot.
			BotGroups = case lists:keyfind(Name, 1, sporkk_core:get_val(BotId, user_groups, [])) of
							false -> [];
							{Name, BotGrps} -> BotGrps
						end,
			% Merge the user's bot group list with the user's global group list, set the username, and return.
			{ok, User#user{
				   username=Name,
				   groups=listutil:remove_duplicates(lists:append(UserCfg#usr_config.groups, BotGroups))
				  }};
		{atomic, Error} ->
			{error, Error}
	end.


%% ============================================================================
%% Internal Functions
%% ============================================================================


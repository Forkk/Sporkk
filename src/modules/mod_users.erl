%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc A module for user account related commands.
%% ============================================================================
-module(mod_users).
-author("Forkk").
-behavior(sporkk_module).
-include("sporkk.hrl").
-include("modules.hrl").

-export([get_info/0, init/1, handle_event/4, handle_command/6, code_change/3, terminate/1]).

-record(state, {}).


get_info() ->
	#mod_info{
	   name = "Users",
	   desc = "A module for user account related commands.",
	   short_desc = "A module for user account related commands.",
	   version = {1, 0, 0},
	   commands =
	   [
		#cmd_info{
		   id = login,
		   name = "login",
		   desc = "Logs you in.",
		   args = [{"Username", required}, {"Password", required}]
		  },
		#cmd_info{
		   id = logout,
		   name = "logout",
		   desc = "Logs you out.",
		   args = []
		  },
		#cmd_info{
		   id = whoami,
		   name = "whoami",
		   desc = "Tells who you are logged in as on the bot.",
		   args = []
		  }
	   ]
	  }.


init(_BotId) ->
	{ok, #state{}}.

handle_event(_Type, _Data, State, _BotId) ->
	{ok, State}.

handle_command(login, Dest, User, [UserName, Password], State, BotId) ->
	case sporkk_authserv:authenticate(BotId, User, UserName, Password) of
		ok ->
			sporkk:send(BotId, Dest, io_lib:format("You have been successfully logged in as ~s!", [UserName]));
		{fail, no_user} ->
			sporkk:send(BotId, Dest, io_lib:format("Login failed: There is no user with the username ~s.", [UserName]));
		{fail, bad_pass} ->
			sporkk:send(BotId, Dest, io_lib:format("Login failed: Wrong password for ~s.", [UserName]));
		{fail, Error} ->
			sporkk:send(BotId, Dest, io_lib:format("Login failed: Unknown error ~s.", [atom_to_list(Error)]))
	end,
	{ok, State};

handle_command(logout, Dest, User, [], State, BotId) ->
	case User#user.username of
		none ->
			sporkk:send(BotId, Dest, "You aren't even logged in!");
		_ ->
			sporkk_authserv:logout(BotId, User),
			sporkk:send(BotId, Dest, "You are now logged out.")
	end,
	{ok, State};

handle_command(whoami, Dest, User, [], State, BotId) ->
	case User#user.username of
		none ->
			sporkk:send(BotId, Dest, "You are nobody!");
		UserName ->
			sporkk:send(BotId, Dest, io_lib:format("You are logged in as ~s.", [UserName]))
	end,
	{ok, State};

handle_command(_Cmd, _Dest, _User, _Args, State, _BotId) ->
	{ok, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_BotId) ->
	ok.



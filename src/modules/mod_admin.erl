%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc A module for bot administrators to manage a bot.
%% ============================================================================
-module(mod_admin).
-author("Forkk").
-behavior(sporkk_module).
-include("sporkk.hrl").
-include("modules.hrl").

-export([get_info/0, init/1, handle_event/4, handle_command/6, code_change/3, terminate/1]).

-record(state, {}).


get_info() ->
	#mod_info{
	   name = "Admin",
	   desc = "A module providing commands for managing the bot.",
	   short_desc = "A module for bot administrators to manage a bot.",
	   version = {1, 0, 0},
	   default_groups=[admin],
	   commands =
	   [
		#cmd_info{
		   id = reconf,
		   name = "reconf",
		   desc = "Reloads the bot's configuration file.",
		   args = []
		  },
		#cmd_info{
		   id = join,
		   name = "join",
		   desc = "Makes the bot join a given channel.",
		   args = [{"Channel", required}]
		  },
		#cmd_info{
		   id = part,
		   name = "part",
		   desc = "Makes the bot part a given channel.",
		   args = [{"Channel", required}]
		  },
		#cmd_info{
		   id = loadmod,
		   name = "loadmod",
		   desc = "Loads the given module.",
		   args = [{"Module", required}]
		  },
		#cmd_info{
		   id = unloadmod,
		   name = "unloadmod",
		   desc = "Unloads the given module.",
		   args = [{"Module", required}]
		  }
	   ]
	  }.


init(_BotId) ->
	{ok, #state{}}.

handle_event(_Type, _Data, State, _BotId) ->
	{ok, State}.

handle_command(reconf, _Dest, _User, [], State, BotId) ->
	gen_server:cast(sporkk:core(BotId), reload_config),
	{ok, State};

handle_command(join, _Dest, _User, [Chan], State, BotId) ->
	sporkk:join(BotId, [Chan]),
	{ok, State};

handle_command(part, _Dest, _User, [Chan], State, BotId) ->
	sporkk:part(BotId, [Chan]),
	{ok, State};

handle_command(loadmod, _Dest, _User, [Mod], State, BotId) ->
	% This converts a string based on user input to an atom. Yet another reason this command should be restricted to administrators only.
	sporkk:load_mod(BotId, list_to_atom(Mod)),
	{ok, State};

handle_command(unloadmod, _Dest, _User, [Mod], State, BotId) ->
	sporkk:unload_mod(BotId, list_to_atom(Mod)),
	{ok, State};

handle_command(_Cmd, _Dest, _User, _Args, State, _BotId) ->
	{ok, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_BotId) ->
	ok.



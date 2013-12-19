%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc A module that says pong when someone says ping.
%% ============================================================================
-module(mod_util).
-author("Forkk").
-behavior(sporkk_module).
-include("modules.hrl").

-export([get_info/0, init/1, handle_event/4, handle_command/6, code_change/3, terminate/1]).

-record(state, {}).


get_info() ->
	#mod_info{
	   name = "Utilities",
	   desc = "A module that provides some basic utility commands such as 'ping', 'commands', 'help', etc.",
	   short_desc = "A basic utility module.",
	   version = {1, 0, 0},
	   commands = 
	   [
		#cmd_info{
		   id = ping,
		   name = "ping",
		   desc = "Makes the bot say \"pong!!!!\".",
		   args = []
		  },
		#cmd_info{
		   id = modules,
		   name = "modules",
		   desc = "Lists loaded modules.",
		   args = []
		  },
		#cmd_info{
		   id = commands,
		   name = "commands",
		   desc = "Lists available commands.",
		   args = []
		  },
		#cmd_info{
		   id = help,
		   name = "help",
		   desc = "Displays help for a given command.",
		   args = [{"Command", false}]
		  }
	   ]
	  }.


init(_BotId) ->
	{ok, #state{}}.

handle_event(_Type, _Data, State, _BotId) ->
	{ok, State}.

handle_command(ping, Source, _User, _Args, State, BotId) ->
	sporkk:send(BotId, Source, "pong" ++ lists:duplicate(random:uniform(13), $!)),
	{ok, State};

handle_command(modules, Source, _User, _Args, State, BotId) ->
	Modules = sporkk_modserv:modules(BotId),
	sporkk:send(BotId, Source, "Loaded modules: " ++ string:join(lists:map(fun(M) -> M#module.name end, Modules), ", ")),
	{ok, State};

handle_command(commands, Source, _User, _Args, State, BotId) ->
	Commands = sporkk_modserv:commands(BotId),
	sporkk:send(BotId, Source, "Available commands: " ++ string:join(lists:map(fun(C) -> C#command.name end, Commands), ", ")),
	{ok, State};

handle_command(help, Source, {Nick, _Acct}, Args, State, BotId) ->
	case Args of
		[CmdName] ->
			Commands = sporkk_modserv:commands(BotId),
			% Find a matching command.
			case lists:filter(fun(C) -> C#command.name == CmdName end, Commands) of
				[Command | _] ->
					% TODO: More descriptive help. Maybe say what module the command is from and its usage info.
					sporkk:send(BotId, Source, Nick ++ ": " ++ Command#command.name ++ " - " ++ Command#command.desc),
					{ok, State};

				[] ->
					sporkk:send(BotId, Source, Nick ++ ": No such command."),
					{ok, State}
			end;

		_ ->
			{bad_usage, State}
	end;

handle_command(_Cmd, _Source, _User, _Args, State, _BotId) ->
	{ok, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_BotId) ->
	ok.


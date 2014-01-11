%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc A module that says pong when someone says ping.
%% ============================================================================
-module(mod_util).
-author("Forkk").
-behavior(sporkk_module).
-include("sporkk.hrl").
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
		% TODO: Move this to the accounts module when it gets added.
		#cmd_info{
		   id = whoami,
		   name = "whoami",
		   desc = "Tells who you are logged in as on the bot.",
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
	sporkk:send(BotId, Source, "Loaded modules: " ++ string:join(lists:map(fun(M) -> M#mod_info.name end, Modules), ", ")),
	{ok, State};

handle_command(commands, Source, _User, _Args, State, BotId) ->
	Commands = sporkk_modserv:commands(BotId),
	sporkk:send(BotId, Source, "Available commands: " ++ string:join(lists:map(fun(C) -> C#cmd_info.name end, Commands), ", ")),
	{ok, State};

handle_command(whoami, Source, User, _Args, State, BotId) ->
	case User#user.username of
		none ->
			sporkk:send(BotId, Source, "You are nobody!");
		UserName ->
			sporkk:send(BotId, Source, io_lib:format("You are logged in as ~s.", [UserName]))
	end,
	{ok, State};

handle_command(help, Source, {Nick, _Acct}, Args, State, BotId) ->
	case Args of
		[CmdName] ->
			Commands = sporkk_modserv:commands(BotId),
			% Find a matching command.
			case lists:filter(fun(C) -> C#cmd_info.name == CmdName end, Commands) of
				[Command | _] ->
					% TODO: More descriptive help. Maybe say what module the command is from and its usage info.
					sporkk:send(BotId, Source, Nick ++ ": " ++ Command#cmd_info.name ++ " - " ++ Command#cmd_info.desc),
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


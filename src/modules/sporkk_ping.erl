%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc A module that says pong when someone says ping.
%% ============================================================================
-module(sporkk_ping).
-author("Forkk").
-behavior(sporkk_module).
-include("modules.hrl").

-export([get_info/0, init/1, handle_event/4, handle_command/6, code_change/3, terminate/1]).

-record(state, {}).


get_info() ->
	#mod_info{
	   name = "Ping",
	   desc = "A module that says pong when someone runs the 'ping' command.",
	   short_desc = "A module that says pong when someone runs the 'ping' command.",
	   version = {1, 0, 0},
	   commands = 
	   [
		#cmd_info{
		   id = ping,
		   name = "ping",
		   desc = "Makes the bot say \"pong!!!!\".",
		   args = []
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
handle_command(_Cmd, _Source, _User, _Args, State, _BotId) ->
	{ok, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_BotId) ->
	ok.


%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc An extremely complex module that makes the bot say "\o/"
%% 		when someone says "woot" or "\o/".
%% ============================================================================
-module(mod_woot).
-author("Forkk").
-behavior(sporkk_module).
-include("modules.hrl").

-export([get_info/0, init/1, handle_event/4, handle_command/6, handle_call/3, code_change/3, terminate/1]).

-record(state, {}).

get_info() ->
	#mod_info{
	   name = "Woot",
	   desc = "A module that says \\o/ when someone says \"woot\" or \"\\o/\".",
	   short_desc = "A module that says \\o/ when someone says \"",
	   version = {1, 0, 0},
	   commands = []
	  }.


init(_BotId) ->
	{ok, #state{}}.

handle_event(message, {Source, _User, "\\o/"}, State, BotId) ->
	sporkk:send(BotId, Source, "\\o/"),
	{ok, State};
handle_event(message, {Source, _User, "woot"}, State, BotId) ->
	sporkk:send(BotId, Source, "\\o/"),
	{ok, State};
handle_event(_Type, _Data, State, _BotId) ->
	{ok, State}.

handle_command(_Cmd, _Source, _User, _Args, State, _BotId) ->
	{ok, State}.


handle_call(_Message, _From, State) ->
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_BotId) ->
	ok.


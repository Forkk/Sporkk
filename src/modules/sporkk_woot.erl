%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc An extremely complex module that makes the bot say "\o/"
%% 		when someone says "woot" or "\o/".
%% ============================================================================
-module(sporkk_woot).
-author("Forkk").
-behavior(sporkk_module).

-export([init/1, handle_event/4, code_change/3, terminate/1]).

-record(state, {}).

init(_BotId) ->
	{ok, #state{}}.

handle_event(message, {{chan, Channel}, _User, "\\o/"}, State, BotId) ->
	sporkk:send(BotId, Channel, "\\o/"),
	{ok, State};
handle_event(message, {{chan, Channel}, _User, "woot"}, State, BotId) ->
	sporkk:send(BotId, Channel, "\\o/"),
	{ok, State};
handle_event(_Type, _Data, State, _BotId) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_BotId) ->
	ok.


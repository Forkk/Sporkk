%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Utility functions.
%% ============================================================================

-module(util).
-author("Forkk").

-export([
		 list_contains/2
		]).

list_contains(List, [Element]) ->
	case length(lists:filter(fun(E) -> E =:= Element end, List)) of
		0 ->
			false;
		_ ->
			true
	end;
list_contains(List, Elements) ->
	case length(lists:filter(fun(E) -> list_contains(Elements, [E]) end, List)) of
		0 ->
			false;
		_ ->
			true
	end.


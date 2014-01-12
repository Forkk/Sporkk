%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Utility functions.
%% ============================================================================

-module(listutil).
-author("Forkk").

-export([
		 list_contains/2, list_contains/3,
		 subtract/2, subtract/3,
		 remove_duplicates/1, remove_duplicates/2
		]).

contains_compare(Val1, Val2, ignore_case) when is_list(Val1) and is_list(Val2) ->
	string:to_lower(Val1) =:= string:to_lower(Val2);
contains_compare(Val1, Val2, _CaseMode) ->
	Val1 =:= Val2.


% @doc Calls list_contains(List, Elements, normal)
list_contains(List, Elements) ->
	list_contains(List, Elements, normal).

% @doc Checks if the given list contains any of the given elements.
list_contains(List, Element, CaseMode) when not is_list(Element) ->
	list_contains(List, [Element], CaseMode);
list_contains(List, [Element], CaseMode) ->
	lists:any(fun(E) -> contains_compare(E, Element, CaseMode) end, List);
list_contains(List, Elements, CaseMode) ->
	lists:any(fun(E) -> list_contains(Elements, [E], CaseMode) end, List).


% @doc Calls subtract(From, Values, normal).
subtract(From, Values) ->
	subtract(From, Values, normal).

% @doc Remove elements in list Values from list From and return the result.
subtract(From, Values, CaseMode) ->
	lists:filter(fun(E) -> not list_contains(Values, [E], CaseMode) end, From).


% @doc Calls remove_duplicates(List, normal).
remove_duplicates(List) ->
	remove_duplicates(List, normal).

% @doc Removes duplicate entries from the list.
remove_duplicates(List, CaseMode) ->
	% FIXME: This doesn't seem very efficient...
	lists:foldl(
	  fun(E, Acc) ->
			  case list_contains(Acc, [E], CaseMode) of
				  false -> lists:append(Acc, [E]);
				  true  -> Acc
			  end
	  end, [], List).


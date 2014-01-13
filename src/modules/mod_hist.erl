%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc A module that logs messages sent to channels.
%% ============================================================================
-module(mod_hist).
-author("Forkk").
-behavior(sporkk_module).
-include("sporkk.hrl").
-include("modules.hrl").

-export([get_info/0, init/1, handle_event/4, handle_command/6, handle_call/3, code_change/3, terminate/1]).

-export([get_logs/2]).

-record(state, {
		  sources=[],
		  % TODO: Make this configurable.
		  maxlines=256
		 }).


%%%%%% API Functions %%%%%%

get_logs(BotId, Source) ->
	gen_server:call(sporkk_module:procname(BotId, ?MODULE), {get_logs, Source}).


%%%%%% Module Functions %%%%%%

get_info() ->
	#mod_info{
	   name = "History",
	   desc = "A module that tracks chat history for each channel the bot is in. Required by other modules that need access to chat history.",
	   short_desc = "Tracks channel chat history.",
	   version = {1, 0, 0},
	   commands =[]
	  }.


init(_BotId) ->
	{ok, #state{}}.

handle_event(message, {Source, User, Message}, State, _BotId) ->
	{Source, LogList} = case lists:keyfind(Source, 1, State#state.sources) of
							false ->
								{Source, []};
							Entry ->
								Entry
						end,
	NewSources = lists:keystore(Source, 1, State#state.sources,
								{Source, append_limit({message, now(), User, Message}, LogList, State#state.maxlines)}),
	{ok, State#state{sources=NewSources}};
handle_event(_Type, _Data, State, _BotId) ->
	{ok, State}.

handle_command(_Cmd, _Dest, _User, _Args, State, _BotId) ->
	{ok, State}.


handle_call({get_logs, Source}, _From, State) ->
	case lists:keyfind(Source, 1, State#state.sources) of
		false ->
			{reply, [], State};
		{Source, LogLines} ->
			{reply, LogLines, State}
	end;
handle_call(_Message, _From, State) ->
	{noreply, State}.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_BotId) ->
	ok.


append_limit(Element, List, Limit) ->
	NewList = [Element|List],
	case length(NewList) > Limit of
		true ->
			% If the list is past max length, reverse it, remove the first entry, and reverse it again.
			[_|NewList2] = lists:reverse(NewList),
			lists:reverse(NewList2);
		false ->
			NewList
	end.


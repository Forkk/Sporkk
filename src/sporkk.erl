%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc API Functions for managing and controlling Sporkk bots.
%% ============================================================================
-module(sporkk).
-author("Forkk").

-export([start/0]).

-export([start/1, stop/1]).

-export([join/2, part/2, part/3]).

-export([send/3]).

-export([connector/1, sender/1, receiver/1, eventmgr/1]).

%% @doc Starts the main application process and its dependencies.
start() ->
	ok = application:start(inets),
	ok = application:start(sasl),
	ok = application:start(mnesia),
	ok = application:start(sporkk).

%% @doc Starts the bot with the given ID.
start(Id) ->
	{ok, Bot} = sporkk_cfg:get_bot(Id),
	case supervisor:start_child({global, sporkk_sup}, sporkk_sup:gen_spec(Bot)) of
		{ok, _Child} ->
			ok;
		{ok, _Child, _Info} ->
			ok;

		{error, already_present} ->
			{error, already_running};
		{error, {already_started, _Child}} ->
			{error, already_running}
	end.

%% @doc Stops the bot with the given ID. Returns ok if the bot stops successfully or isn't running.
stop(Id) ->
	case supervisor:terminate_child({global, sporkk_sup}, Id) of
		ok ->
			ok = supervisor:delete_child({global, sporkk_sup}, Id),
			ok;
		{error, not_found} ->
			ok
	end.


%% @doc Tells the bot with the given ID to join the given channels and adds the channels to the bot's DB entry.
join(Id, Channels) ->
	sporkk_cfg:add_bot_chans(Id, Channels),
	gen_server:cast(sender(Id), {join, Channels}).

%% @doc Tells the bot with the given ID to part from the given channels and removes the channels from the bot's DB entry.
part(Id, Channels, Reason) ->
	sporkk_cfg:remove_bot_chans(Id, Channels),
	gen_server:cast(sender(Id), {part, Channels, Reason}).
part(Id, Channels) ->
	part(Id, Channels, "I was told to leave.").


%% @doc Tells the bot with the given ID to send the given message to the given destination.
send(Id, Dest, Message) ->
	gen_server:cast(sender(Id), {notice, Dest, Message}).


%% ============================================================================
%% Some hacky functions for finding process names of a bot's components.
%% ============================================================================
%% @doc Gets the connector process name for the given bot ID.
connector(Id) when is_atom(Id) ->  {global, combine_atoms(Id, connector)}.

%% @doc Gets the sender process name for the given bot ID.
sender(Id) when is_atom(Id) ->     {global, combine_atoms(Id, sender)}.

%% @doc Gets the receiver process name for the given bot ID.
receiver(Id) when is_atom(Id) ->   {global, combine_atoms(Id, receiver)}.

%% @doc Gets the event manager process name for the given bot ID.
eventmgr(Id) when is_atom(Id) ->   {global, combine_atoms(Id, eventmgr)}.


%% ============================================================================
%% Internal Functions
%% ============================================================================
combine_atoms(First, Second) when is_atom(First), is_atom(Second) ->
	% Nuclear fusion! Smash those atoms together!
	list_to_atom(atom_to_list(First) ++ "_" ++ atom_to_list(Second)).


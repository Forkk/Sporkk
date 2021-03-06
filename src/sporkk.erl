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

-export([load_mod/2, unload_mod/2]).

-export([send/3, notice/3, log_info/2]).

-export([core/1, connector/1, sender/1, receiver/1, modserv/1, modsup/1, authserv/1]).

%% @doc Starts the main application process and its dependencies.
start() ->
	{ok, _Deps} = application:ensure_all_started(sporkk).

%% @doc Starts the bot with the given ID.
start(BotId) ->
	case supervisor:start_child({global, sporkk_sup}, sporkk_sup:gen_spec(BotId)) of
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
	gen_server:cast(sender(Id), {join, Channels}).

%% @doc Tells the bot with the given ID to part from the given channels and removes the channels from the bot's DB entry.
part(Id, Channels, Reason) ->
	gen_server:cast(sender(Id), {part, Channels, Reason}).
part(Id, Channels) ->
	part(Id, Channels, "I was told to leave.").


%% @doc Tells the given bot to load a module with the given ID.
load_mod(BotId, ModId) ->
	gen_server:cast(modserv(BotId), {load_mod, ModId}).

%% @doc Tells the given bot to unload the given module.
unload_mod(BotId, ModId) ->
	gen_server:cast(modserv(BotId), {unload_mod, ModId}).

%% @doc Tells the bot with the given ID to send the given message to the given destination.
send(Id, Dest, Message) ->
	gen_server:cast(sender(Id), {privmsg, Dest, Message}).

%% @doc Tells the bot with the given ID to send the given notice to the given destination.
notice(Id, Dest, Message) ->
	gen_server:cast(sender(Id), {notice, Dest, Message}).

%% @doc Sends the given message to the info logging system.
%% This will send the message to all the channels listed in the bot's 'log_chans' extra.
log_info(Id, Message) ->
	gen_server:cast(sender(Id), {log, info, Message}).


%% ============================================================================
%% Some hacky functions for finding process names of a bot's components.
%% ============================================================================

%% @doc Gets the core process name for the given bot ID.
core(Id) when is_atom(Id) ->       {global, {Id, core}}.

%% @doc Gets the connector process name for the given bot ID.
connector(Id) when is_atom(Id) ->  {global, {Id, connector}}.

%% @doc Gets the sender process name for the given bot ID.
sender(Id) when is_atom(Id) ->     {global, {Id, sender}}.

%% @doc Gets the receiver process name for the given bot ID.
receiver(Id) when is_atom(Id) ->   {global, {Id, receiver}}.

%% @doc Gets the module server process name for the given bot ID.
modserv(Id) when is_atom(Id) ->    {global, {Id, modserv}}.

%% @doc Gets the module supervisor process name for the given bot ID.
modsup(Id) when is_atom(Id) ->     {global, {Id, modsup}}.

%% @doc Gets the authentication server process name for the given bot ID.
authserv(Id) when is_atom(Id) ->   {global, {Id, authserv}}.


%% ============================================================================
%% Internal Functions
%% ============================================================================



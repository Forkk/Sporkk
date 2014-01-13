%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc The bot's core process.
%%      Keeps track of state and configuration information.
%% ============================================================================
-module(sporkk_core).
-behavior(gen_server).
-include("sporkk.hrl").

%% API Functions
-export([
		 start_link/1,
		 get_val/2, get_val/3,
		 get_mod_config/2, get_mod_groups/2
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
% botid is the bot's ID.
% modules is a list of {ModId, MonitorId} tuples, where ModId is the module's 
% Erlang module name, and MonitorId is the module's process monitor ref.
-record(state, {
		  bot
		 }).

%% ============================================================================
%% API Functions
%% ============================================================================

% @doc Starts the core.
start_link(BotId) ->
	Bot = sporkk_cfg:get_bot_entry(BotId),
	gen_server:start_link(sporkk:core(BotId), ?MODULE, [Bot], []).


% @doc Gets the given config value for the given bot. Returns Default if it can't be found.
get_val(BotId, Key, Default) ->
	try get_val(BotId, Key) of
		Value -> Value
	catch
		no_val -> Default
	end.

% @doc Gets the given config value for the given bot. Throws 'no_val' if it can't be found.
get_val(BotId, Key) ->
	case gen_server:call(sporkk:core(BotId), {get_config_val, Key}) of
		{error, no_val} -> throw(no_val);
		{ok, Value} -> Value
	end.


%% @doc Gets a list of groups the given mod belongs to on the given bot.
get_mod_groups(BotId, Module) ->
	case lists:keyfind(Module, 1, get_val(BotId, modules, [])) of
		false ->
			[all];
		{Module, Groups, _Config} ->
			Groups
	end.

%% @doc Gets a list of config keys for the given module.
get_mod_config(BotId, Module) ->
	case lists:keyfind(Module, 1, get_val(BotId, modules, [])) of
		false ->
			[all];
		{Module, _Groups, Config} ->
			Config
	end.


%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Initializes the server's state.
%% ----------------------------------------------------------------------------
init([Bot]) ->
	{ok, #state{bot=Bot}}.

%% ----------------------------------------------------------------------------
%% Call messages
%% ----------------------------------------------------------------------------
% Get a config value.
handle_call({get_config_val, Key}, _From, State) ->
	{reply, get_config_val(State, Key), State};

handle_call(_Request, _From, State) ->
	{ok, State}.


%% ----------------------------------------------------------------------------
%% Cast messages
%% ----------------------------------------------------------------------------
handle_cast(reload_config, State) ->
	{Id, _Options} = State#state.bot,
	sporkk:log_info(Id, "Reloading config..."),
	{noreply, State#state{bot=sporkk_cfg:get_bot_entry(Id)}};

handle_cast(_Request, State) ->
	{noreply, State}.


%% ----------------------------------------------------------------------------
%% @doc Handle non cast/call messages.
%% ----------------------------------------------------------------------------
handle_info(_Request, State) ->
	{noreply, State}.


%% ----------------------------------------------------------------------------
%% @doc Called when the server terminates.
%% ----------------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%% ----------------------------------------------------------------------------
%% @doc Called when code changes.
%% ----------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ============================================================================
%% Internal Functions
%% ============================================================================

get_config_val(#state{bot={_Id, Options}}, Key) ->
	case lists:keyfind(Key, 1, Options) of
		false -> {error, no_val};
		{Key, Value} -> {ok, Value}
	end.


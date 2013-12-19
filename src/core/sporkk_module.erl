%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Module for Sporkk's bot module behavior.
%% ============================================================================
-module(sporkk_module).
-behavior(gen_server).
-include("modules.hrl").

-export([start_link/2]).

-export([behaviour_info/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {botid, module, modstate}).

behaviour_info(callbacks) ->
	% Sporkk modules need the following callback functions.
	[
	 % 'init(BotId)' is used to initialize the module's state and anything else the module needs to operate.
	 %
	 % Possible return values: {ok, State} | {error, Reason}
	 {init, 1},

	 % get_info() -> {ok, ModInfo}
	 % 	Types:
	 % 		ModInfo = #mod_info{}
	 %
	 % Called after init. This should be used to give the system some information about the module.
	 % It should return a #mod_info record specifying information such as the module's name, version, commands, etc.
	 %
	 {get_info, 0},

	 % handle_event(EventType, EventData, State, BotId) -> {ok, NewState}
	 % 	Types:
	 % 		EventType = atom()
	 % 		EventData = term()
	 % 		State = term()
	 % 		BotId = atom()
	 % 		NewState = term()
	 %
	 % This is called for any sort of event that happens on the IRC. For example, users sending 
	 % messages to a channel or to the bot, commands, mode changes, joins and parts, etc.
	 % Here are some of the events that Sporkk will pass to modules:
	 %
	 % message: {Source, User, Message}
	 % 	Types:
	 % 		Source = string() - A string specifying either the channel the message was from or the sender's nick in the case of a PM.
	 % 							If the message was from a channel, this string will always start with a '#' character.
	 % 		User = {Nick, Account} - A tuple of the nick and account (if applicable) of the user who sent the message.
	 % 		Message = string() - The message the user sent.
	 % Called when a user sends a message in a channel or directly to the bot.
	 %
	 {handle_event, 4},

	 % handle_command(CommandId, Source, User, Args, State, BotId) -> {ok, NewState} | {bad_usage, NewState}
	 % 	Types:
	 % 		CommandId = atom() - An atom identifying the command. This will be the atom ID that the command was registered with.
	 % 		Source = string() - A string specifying either the channel the message was from or the sender's nick in the case of a PM.
	 % 							If the message was from a channel, this string will always start with a '#' character.
	 % 		User = {Nick, Account} - A tuple of the nick and account (if applicable) of the user who sent the command.
	 % 		Args = [string()] - A list of strings representing the command's arguments.
	 % 		State = term() - The module's state.
	 % 		BotId = atom() - The module's bot's ID.
	 %
	 {handle_command, 6},

	 % code_change(OldVsn, State, Extra) -> {ok, NewState}
	 % Called when this module's code changes. See OTP gen_event documentation for more info.
	 {code_change, 3},

	 % 'terminate(State, BotId)' is called when the module is terminated.
	 % The module is considered to be terminated whenever it crashes, is unloaded, or the bot is shut down.
	 {terminate, 1}
	].


%% ============================================================================
%% Helper Functions
%% ============================================================================

%% @doc Registers the given module module with the given bot ID.
start_link(BotId, Module) ->
	gen_server:start_link(?MODULE, [BotId, Module], []).


%% ============================================================================
%% Internal Functions - gen_event callbacks.
%% ============================================================================

%% @doc Initializes the module state.
init([BotId, Module]) ->
	case Module:init(BotId) of
		{ok, State} ->
			{ok, #state{botid=BotId, module=Module, modstate=State}};
		{error, Reason} ->
			{error, Reason}
	end.

%% @doc Handles a cast from the module server.
handle_cast({event, EventType, EventData}, State) ->
	{ok, NewModState} = (State#state.module):handle_event(EventType, EventData, State#state.modstate, State#state.botid),
	{noreply, State#state{modstate=NewModState}};
handle_cast({command, CommandId, Source, User, Args}, State) ->
	{ok, NewModState} = (State#state.module):handle_command(CommandId, Source, User, Args, State#state.modstate, State#state.botid),
	{noreply, State#state{modstate=NewModState}};
handle_cast(_EventData, State) ->
	error_logger:warning_msg("Unknown module cast (bad format): ~w~n", [_EventData]),
	{noreply, State}.

%% @doc Called when code changes.
code_change(OldVsn, State, Extra) ->
	{ok, NewModState} = (State#state.module):code_change(OldVsn, State#state.modstate, Extra),
	{ok, State#state{modstate=NewModState}}.

%% @doc Called when the event handler is removed from the router. Calls the callback module's terminate function. 
terminate(_Arg, State) ->
	(State#state.module):terminate(State#state.modstate).


% Ignore these...
handle_call(_Request, _From, State) -> {noreply, State}.
handle_info(_Info, State) -> {ok, State}.


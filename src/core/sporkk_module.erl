%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Module for Sporkk's bot module behavior.
%% ============================================================================
-module(sporkk_module).
-behavior(gen_event).

-export([register_mod/2]).

-export([behaviour_info/1]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {botid, module, modstate}).

behaviour_info(callbacks) ->
	% Sporkk modules need the following callback functions.
	[
	 % 'init(BotId)' is used to initialize the module's state and anything else the module needs to operate.
	 %
	 % Possible return values: {ok, State} | {error, Reason}
	 {init, 1},

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
	 % 		Source = {SourceType, SourceName} - A tuple specifying either the channel the message was from or the sender's nick in the case of a PM.
	 % 			SourceType = chan | msg | undefined - An atom indicating what type of source this message came from.
	 % 			SourceName = string() - A string indicating the name of the source of this message.
	 % 		User = {Nick, Account} - A tuple of the nick and account (if applicable) of the user who sent the message.
	 % 		Message = string() - The message the user sent.
	 % Called when a user sends a message in a channel or directly to the bot.
	 %
	 % 
	 % command: {Source, User, CommandId, Args}
	 % 	Types:
	 % 		Source = {SourceType, SourceName} - A tuple specifying either the channel the command was from or the sender's nick in the case of a PM.
	 % 			SourceType = chan | msg | undefined - An atom indicating what type of source this command came from.
	 % 			SourceName = string() - A string indicating the name of the source of this command.
	 % 		User = {Nick, Account} - A tuple of the nick and account (if applicable) of the user who sent the command.
	 % 		CommandId = atom() - The command's ID atom. This is the ID atom that was specified when the command was registered.
	 % 		Args = [string()] - A list of strings representing the command's arguments.
	 %
	 {handle_event, 4},

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
register_mod(BotId, Module) ->
	gen_event:add_handler(sporkk:eventmgr(BotId), ?MODULE, [BotId, Module]).


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

%% @doc Handles an event from the event manager and passes it to the callback module.
handle_event({EventType, EventData}, State) ->
	{ok, NewModState} = (State#state.module):handle_event(EventType, EventData, State#state.modstate, State#state.botid),
	{ok, State#state{modstate=NewModState}};
handle_event(_EventData, State) ->
	error_logger:warning_msg("Unknown module event (bad format): ~w~n", [_EventData]),
	{ok, State}.

%% @doc Called when code changes.
code_change(OldVsn, State, Extra) ->
	{ok, NewModState} = (State#state.module):code_change(OldVsn, State#state.modstate, Extra),
	{ok, State#state{modstate=NewModState}}.

%% @doc Called when the event handler is removed from the event manager. Calls the callback module's terminate function. 
terminate(_Arg, State) ->
	(State#state.module):terminate(State#state.modstate).


% Ignore these...
handle_call(_Request, State) -> {ok, wat, State}.
handle_info(_Info, State) -> {ok, State}.


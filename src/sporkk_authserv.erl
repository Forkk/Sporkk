%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Sporkk's authentication server.
%%      This handles logging in users and their sessions.
%% ============================================================================
-module(sporkk_authserv).
-behavior(gen_server).
-include("sporkk.hrl").
-include("db.hrl").

%% API Functions
-export([start_link/1, authenticate/4, get_username/2, get_user/2, logout/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% State record
-record(state, {
		  botid,
		  sessions=[]
		 }).


%% ============================================================================
%% API Functions
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Starts the authentication server.
%% ----------------------------------------------------------------------------
start_link(BotId) ->
	gen_server:start_link(sporkk:authserv(BotId), ?MODULE, [BotId], []).

%% @doc Sends a call to the auth manager for the given bot ID to authenticate
%%      the given hostmask as the given username with the given password.
%%      The password should be passed to this function in plaintext. It will
%%      be hashed with sha512 before being sent to the auth server.
%%      Returns 'ok' on success and {fail, Reason} on failure.
authenticate(BotId, #user{ident=Ident, hostname=Host}, UserName, Pass) ->
	gen_server:call(sporkk:authserv(BotId), {auth, {Ident, Host}, UserName, crypto:hash(sha512, Pass)}).

%% @doc Gets the username of the user with the given hostmask.
get_username(BotId, #user{ident=Ident, hostname=Host}) ->
	gen_server:call(sporkk:authserv(BotId), {get_user, {Ident, Host}}).

%% @doc Returns a 'user' record filled out with the user's account info.
get_user(BotId, User) ->
	UserName = get_username(BotId, User),
	case sporkk_cfg:get_user(BotId, UserName, User) of
		{ok, UInfo} ->
			UInfo;
		{error, no_user} ->
			User
	end.

%% @doc Logs out the given username or hostmask.
logout(BotId, #user{username=UserName}) ->
	logout(BotId, UserName);
logout(BotId, Who) ->
	ok = gen_server:cast(sporkk:authserv(BotId), {logout, Who}),
	ok.

%% ============================================================================
%% Callbacks
%% ============================================================================

%% ----------------------------------------------------------------------------
%% @doc Initializes the server's state.
%% ----------------------------------------------------------------------------
init([BotId]) ->
	{ok, #state{botid=BotId}}.

%% ----------------------------------------------------------------------------
%% @spec handle_call(Request, From, State) -> {reply, Reply, State}          |
%%                                            {reply, Reply, State, Timeout} |
%%                                            {noreply, State}               |
%%                                            {noreply, State, Timeout}      |
%%                                            {stop, Reason, Reply, State}   |
%%                                            {stop, Reason, State}
%% @doc Handles call messages.
%% ----------------------------------------------------------------------------
%% @doc Attempt to authenticate the given hostmask as the given user using the given password.
%%      The password should be hashed with sha512 before being sent.
%%      Replies 'ok' on success and {fail, Reason} on failure.
handle_call({auth, HostMask, UserName, Pass}, _From, State) ->
	case mnesia:transaction(
		   fun() ->
				   case mnesia:read({usr_config, UserName}) of
					   [User] ->
						   User#usr_config.pass;
					   [] ->
						   no_user
				   end
		   end)
	of
		{atomic, no_user} ->
			{reply, {fail, no_user}, State};
		{atomic, PassHash} ->
			case passwd:check_pass(Pass, PassHash) of
				ok ->
					% Log the user in.
					{reply, ok, log_in_user(HostMask, UserName, State)};
				fail ->
					{reply, {fail, bad_pass}, State}
			end
	end;

%% @doc Checks who the given hostmask is logged in as and replies with their username or 'nobody' if they aren't logged in.
handle_call({get_user, {Ident, Host}}, _From, State) ->
	% First, we need to find them in the sessions list.
	case lists:keyfind({Ident, Host}, 1, State#state.sessions) of
		false ->
			% The user isn't logged in. 
			{reply, nobody, State};
		{{Ident, Host}, UserName, _Timestamp} ->
			% The user is logged in.
			{reply, UserName, State}
	end;

% Mostly just for debugging.
handle_call(get_sessions, _From, State) ->
	{reply, State#state.sessions, State};

handle_call(_Request, _From, State) ->
	{noreply, State}.

%% ----------------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State}             |
%%                                  {noreply, State, Timeout}    |
%%                                  {stop, Reason, State}
%% @doc Handles cast messages.
%% ----------------------------------------------------------------------------
%% @doc Logs out the given hostname.
handle_cast({logout, {Ident, Host}}, State) ->
	NewSessions = lists:filter(fun({{SIdent, SHost}, _, _}) -> (SIdent =/= Ident) or (SHost =/= Host) end, State#state.sessions),
	{noreply, State#state{sessions=NewSessions}};

%% @doc Logs out the given username.
handle_cast({logout, UserName}, State) ->
	NewSessions = lists:filter(fun({_, SUsrName, _}) -> SUsrName =/= UserName end, State#state.sessions),
	{noreply, State#state{sessions=NewSessions}};


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
%% @doc Logs in the given hostmask as the given username and returns the resulting state.
%% If any other hostmasks are authed as the given username, they will be logged out.
log_in_user({Ident, Host}, UserName, State) ->
	% Remove others logged in as the given user.
	NewSessions = lists:filter(fun({_, SessionUserName, _}) -> SessionUserName =/= UserName end, State#state.sessions),
	% Add the current user.
	State#state{sessions=lists:append(NewSessions, [{{Ident, Host}, UserName, now()}])}.


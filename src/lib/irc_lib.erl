%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Utility functions for IRC.
%% A lot of this is from https://github.com/wrboyce/erb/blob/master/src/lib/irc_lib.erl
%% ============================================================================

-module(irc_lib).
-author("Forkk").

-export([operation_to_atom/1]).
-export([pong/1, nick/1, register/4, join/1, privmsg/2, kick/2, kick/3, mode/2, quiet/2, ban/2, whois/1]).

%% -------------------------------------------------------------------
%% @spec operation_to_atom() -> atom
%% @doc Parse an Operation into a representive atom as RFC2812
%% -------------------------------------------------------------------
operation_to_atom("001") ->
	rpl_welcome;
operation_to_atom("002") ->
	rpl_yourhost;
operation_to_atom("003") ->
	rpl_created;
operation_to_atom("004") ->
	rpl_myinfo;
operation_to_atom("005") ->
	rpl_bounce;

operation_to_atom("200") ->
	rpl_tracelink;
operation_to_atom("201") ->
	rpl_traceconnecting;
operation_to_atom("202") ->
	rpl_tracehandshake;
operation_to_atom("203") ->
	rpl_traceunknown;
operation_to_atom("204") ->
	rpl_traceoperator;
operation_to_atom("205") ->
	rpl_traceuser;
operation_to_atom("206") ->
	rpl_traceserver;
operation_to_atom("207") ->
	rpl_traceservice;
operation_to_atom("208") ->
	rpl_tracenewtype;
operation_to_atom("209") ->
	rpl_traceclass;
operation_to_atom("210") ->
	rpl_tracereconnect;
operation_to_atom("211") ->
	rpl_tracestatslinkinfo;
operation_to_atom("330") ->
        rpl_whoisaccount;
operation_to_atom("433") ->
	err_nicknameinuse;

operation_to_atom("JOIN") ->
	join;
operation_to_atom("PRIVMSG") ->
	privmsg;

operation_to_atom(Other) ->
	Other.


%% -------------------------------------------------------------------
%% @spec pong() -> string
%% @doc Reply to a server PING
%% -------------------------------------------------------------------
pong(Server) ->
	"PONG " ++ Server.

%% -------------------------------------------------------------------
%% @spec nick() -> string
%% @doc Change nick.
%% -------------------------------------------------------------------
nick(Nick) ->
	"NICK " ++ Nick.

%% -------------------------------------------------------------------
%% @spec register() -> string
%% @doc Register with the server
%% -------------------------------------------------------------------
register(User, Host, Server, RealName) ->
	"USER " ++ User ++ " " ++ Host ++ " " ++ Server ++ " :" ++ RealName.

%% -------------------------------------------------------------------
%% @spec join(Result, Channels) -> string
%% @doc Join the channel(s)
%% -------------------------------------------------------------------
join(Channels) ->
    join([], Channels).
join(Result, []) ->
    string:join(Result, "\r\n");
join(Result, [Channel|Channels]) ->
    join(lists:append(Result, ["JOIN " ++ Channel]), Channels).

%% -------------------------------------------------------------------
%% @spec privmsg(Dest, Msg) -> string
%% @doc Send Msg to Dest(ination) via PRIVMSG
%% -------------------------------------------------------------------
privmsg(Dest, Msg) ->
	"PRIVMSG " ++ Dest ++ " :" ++ Msg.

%% -------------------------------------------------------------------
%% @spec kick(Chan, Nick | Chan, Nick, Reason) -> string
%% @doc Send Msg to Dest(ination) via PRIVMSG
%% -------------------------------------------------------------------
kick(Chan, Nick) ->
	kick(Chan, Nick, Nick).
kick(Chan, Nick, Reason) ->
	"KICK " ++ Chan ++ " " ++ Nick ++ " :" ++ Reason.

%% -------------------------------------------------------------------
%% @spec mode(Dest, Mode) -> string
%% @doc Change mode for Dest(ination)
%% -------------------------------------------------------------------
mode(Dest, Mode) ->
	"MODE " ++ Dest ++ " " ++ Mode.

%% -------------------------------------------------------------------
%% @spec shun(Chan, Mask) -> string
%% @doc Prevent users matching Mask from talking in Chan
%% -------------------------------------------------------------------
quiet(Chan, Mask) ->
	mode(Chan, "+q " ++ Mask).

%% -------------------------------------------------------------------
%% @spec ban(Chan, Mask) -> string
%% @doc Ban mask from joining Chan
%% -------------------------------------------------------------------
ban(Chan, Mask) ->
	mode(Chan, "+b " ++ Mask).

%% -------------------------------------------------------------------
%% @spec whois(Nick) -> string
%% @doc Lookup user info from the server
%% -------------------------------------------------------------------
whois(Nick) ->
    "WHOIS " ++ Nick.


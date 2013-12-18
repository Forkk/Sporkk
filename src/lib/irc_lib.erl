%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Utility functions for IRC.
%% A lot of this is from https://github.com/wrboyce/erb/blob/master/src/lib/irc_lib.erl
%% ============================================================================

-module(irc_lib).
-author("Forkk").
-include("sporkk.hrl").

-export([parse_message/3]).
-export([command_to_atom/1]).
-export([pong/1, nick/1, register/4, join/1, part/1, part/2, notice/2, privmsg/2, kick/2, kick/3, mode/2, quiet/2, ban/2, whois/1]).

%% -------------------------------------------------------------------
%% @spec command_to_atom() -> atom
%% @doc Parse an Operation into a representive atom as RFC2812
%% -------------------------------------------------------------------
command_to_atom("001") ->
	reply_welcome;
command_to_atom("002") ->
	reply_yourhost;
command_to_atom("003") ->
	reply_created;
command_to_atom("004") ->
	reply_myinfo;
command_to_atom("005") ->
	reply_bounce;

command_to_atom("200") ->
	reply_tracelink;
command_to_atom("201") ->
	reply_traceconnecting;
command_to_atom("202") ->
	reply_tracehandshake;
command_to_atom("203") ->
	reply_traceunknown;
command_to_atom("204") ->
	reply_traceoperator;
command_to_atom("205") ->
	reply_traceuser;
command_to_atom("206") ->
	reply_traceserver;
command_to_atom("207") ->
	reply_traceservice;
command_to_atom("208") ->
	reply_tracenewtype;
command_to_atom("209") ->
	reply_traceclass;
command_to_atom("210") ->
	reply_tracereconnect;
command_to_atom("211") ->
	reply_tracestatslinkinfo;
command_to_atom("330") ->
	reply_whoisaccount;
command_to_atom("433") ->
	error_nicknameinuse;

command_to_atom("PING") ->
	ping;
command_to_atom("JOIN") ->
	join;
command_to_atom("PRIVMSG") ->
	privmsg;

command_to_atom(Other) ->
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
	"JOIN " ++ string:join(Channels, ",").

%% -------------------------------------------------------------------
%% @spec part(Result, Channels) -> string
%% @doc Part the channel(s)
%% -------------------------------------------------------------------
part(Channels) ->
	part(Channels, "Leaving").
part(Channels, Reason) ->
	"PART " ++ string:join(Channels, ",") ++ " :" ++ Reason.

%% -------------------------------------------------------------------
%% @spec privmsg(Dest, Msg) -> string
%% @doc Send Msg to Dest(ination) via PRIVMSG
%% -------------------------------------------------------------------
privmsg(Dest, Msg) ->
	"PRIVMSG " ++ Dest ++ " :" ++ Msg.

%% -------------------------------------------------------------------
%% @spec notice(Dest, Msg) -> string
%% @doc Send Msg to Dest(ination) via NOTICE
%% -------------------------------------------------------------------
notice(Dest, Msg) ->
	"NOTICE " ++ Dest ++ " :" ++ Msg.

%% -------------------------------------------------------------------
%% @spec kick(Chan, Nick | Chan, Nick, Reason) -> string
%% @doc Kick the given user from the given channel.
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

%% ----------------------------------------------------------------------------
%% @doc Parses a line from the IRC server into a #line record.
%% ----------------------------------------------------------------------------
parse_message(Bot, DateTime, Line) when is_binary(Line) ->
	parse_message(Bot, DateTime, binary_to_list(Line));

parse_message(Bot, DateTime, Line) ->
	{User, Command, Args, Body} = parse_line(Line),
	Dest = if length(Args) >= 1 -> lists:nth(1, Args); true -> undefined end,
	{ok, #line{
			bot = Bot,
			datetime = DateTime,
			source = User,
			command = command_to_atom(Command),
			destination = Dest,
			args = Args,
			body = Body
		   }}.

% Lines beginning with : have a prefix. Parse that first.
parse_line([$: | Line]) ->
	% Split the line into two parts at the first space.
	% This separates the prefix from the rest of the command.
	{Prefix, Content} = lists:split(string:chr(Line, $ ), Line),
	{Command, Args, Body} = parse_cmd(Content),
	{Prefix, Command, Args, Body};

parse_line(Line) ->
	{Command, Args, Body} = parse_cmd(Line),
	{undefined, Command, Args, Body}.

% Parses the line's command, arguments, and body. Returns them in a tuple {Command, Args, Body}.
parse_cmd(Line) ->
	% First, we need to find the body and separate it from the rest of the message.
	% We do this by splitting the line in to two parts at the first " :".
	{Header, Body} = case string:str(Line, " :") of
						 0 ->
							 {Line, ""};
						 N ->
							 lists:split(N+1, Line)
					 end,

	% Next, we split the string at spaces and take the command as the first entry in that list.
	[Command | Args] = string:tokens(Header, " "),

	% Finally, return the tuple with the command and arguments.
	{string:strip(Command), Args, Body}.


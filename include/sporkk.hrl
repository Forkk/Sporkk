%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Record definitions for Sporkk.
%% ============================================================================

%% @doc Record for information about an active bot.
-record(bot, 
		{
		 id,
		 network,
		 nick,
		 channels,
		 modules,
		 extras
		}).

%% @doc Record representing a line from the IRC server.
-record(line, 
		{
		 % The bot ID of the bot receiving this line.
		 botid,
		 % A datetime structure specifying when the line was received.
		 datetime,
		 % A tuple in the format {Nick, Ident, Host, Account}, specifying who this line came from.
		 % The "Account" value specifies the user's account ID. If the user has no account, it will be 'none'.
		 % If the line isn't from a user, this will be 'none'.
		 sender=none,
		 % An atom identifying the command in this line.
		 command,
		 % Where this line was sent to (a channel or the bot's nick).
		 % 'none' if not applicable.
		 dest,
		 % Extra command arguments.
		 args,
		 % The line's body content. For example, for a PRIVMSG, this is the message content.
		 body
		}).

%% @doc Recoord for information about a user.
-record(user,
		{
		 % The user's nick.
		 nick,
		 % The user's ident.
		 ident,
		 % The user's hostname.
		 hostname,
		 % The user's username on Sporkk.
		 username,
		 % The user's groups on the bot for which this record is being used.
		 groups
		}).


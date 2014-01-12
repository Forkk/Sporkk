%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Record definitions for Sporkk.
%% ============================================================================

%% @doc Record for information about a bot.
-record(bot, 
		{
		 % The bot's atom identifier.
		 id,
		 % A list of nicks the bot should try.
		 nicks,
		 % A list of servers the bot should try to connect to.
		 servers,
		 % A list of channels the bot should join by default.
		 channels,
		 % A list of modules to load at startup.
		 modules,
		 % A list of channels to send status alerts to. For example, when a module crashes.
		 log_chans
		}).

%% @doc Record representing a line from the IRC server.
-record(line, 
		{
		 % The bot ID of the bot receiving this line.
		 botid,
		 % A datetime structure specifying when the line was received.
		 datetime,
		 % A 'user' record specifying who sent this line.
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
		 username=none,
		 % The user's groups on the bot for which this record is being used.
		 groups=[]
		}).


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
		 extras
		}).

%% @doc Record representing a line from the IRC server.
-record(line, 
		{
		 bot,
		 datetime,
		 source,
		 command,
		 destination,
		 args,
		 body,
		 user
		}).


%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Record definitions for Sporkk.
%% ============================================================================
-author("Forkk").

%% @doc Record for information about an active bot.
-record(bot, {id,
			  network,
			  nick,
			  channels,
			  sender=undefined,
			  router=undefined
			 }).

%% @doc Record representing a line from the IRC server.
-record(line, {bot,
			   datetime,
			   origin,
			   operation,
			   destination,
			   options,
			   body,
			   account}).


%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Sporkk's database records.
%% ============================================================================

% Record for storing user information.
-record(usr_config, {
		  % The user's username.
		  name,
		  % The user's hashed, salted password.
		  pass,
		  % The user's global groups - groups the user is in for every bot.
		  % Primarily used for "owners" of the bot.
		  groups=[],
		  % Extra information.
		  extras=[]
		 }).



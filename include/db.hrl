%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Sporkk's database records.
%% ============================================================================

% Record for storing information about a bot's configuration.
-record(bot_config, {
		  % An ID that uniquely identifies this bot.
		  id,
		  % The ID of the network that this bot connects to.
		  network,
		  % The bot's nick.
		  nick,
		  % Set of strings representing the channels this bot will connect to.
		  channels=[],
		  % Set of atoms representing the modules the bot should load on startup.
		  modules=[],
		  % List of {User, GroupId} tuples, mapping users' nickserv accounts to their user group for this bot.
		  usergrps=[],
		  % List of {Module, [GroupId|_]} or {module, all} tuples for specifying which users can use which modules.
		  modgrps=[],
		  % List of extra configuration options.
		  extra=[],
		  % If the bot is enabled, 'true', else 'false'.
		  enabled=true
		 }).

% Record for storing information about an IRC network.
-record(net_config, {
		  % An ID that uniquely identifies this network.
		  id,
		  servers=[]
		 }).

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



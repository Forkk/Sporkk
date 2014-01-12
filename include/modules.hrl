%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Record definitions used by Sporkk's bot modules.
%% ============================================================================

%% @doc Record that holds information about a module.
%% 		Fields:
%% 			name = string() - A short, human readable name for this module.
%% 			desc = string() - A string describing this module and what it does.
%% 			short_desc = string() - A shorter description of the module.
%% 			version = {Major, Minor, Rev} - A version info tuple describing this module's version.
%% 			commands = list() - A list of #command records containing information about the commands this module provides.
-record(mod_info,
		{
		 name,
		 desc,
		 short_desc,
		 version,
		 default_groups=[all],
		 commands
		}).

%% @doc Record that holds information about a command.
%% 		Fields:
%% 			id = atom() - An atom identifying this command. This will be used to identify the command when passing the command event to modules.
%% 			name = string() - The string name of the command. This is a case-insensitive string used to allow users to call the command.
%% 			desc = string() - The command's description. This should identify what the command does.
%% 			args = [{Name, Optional}] - A list of argument tuples describing the arguments the command takes. Currently only for display purposes.
%% 				Name = string() - The string name of the argument.
%% 				Optional = atom() - 'optional' if arg is optional, else 'required'.
-record(cmd_info,
		{
		 id,
		 name,
		 desc,
		 args
		}).


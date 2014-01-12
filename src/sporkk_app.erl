%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Main app module for Sporkk.
%% ============================================================================

-module(sporkk_app).
-author("Forkk").
-behavior(application).

-export([start/2, stop/1]).

%% Starts the application's core supervisor.
start(_Type, _Args) ->
	% Start the main supervisor.
	case sporkk_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			{error, Error}
	end.

%% Called to stop the application.
stop(_State) ->
	exit(whereis(sporkk_sup), shutdown).


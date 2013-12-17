%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Main app module for Sporkk.
%% ============================================================================

-module(sporkk_app).
-author("Forkk").
-behavior(application).

-export([start/0, start/2, stop/1]).

start() ->
	ok = application:start(inets),
	ok = application:start(sasl),
	ok = application:start(mnesia),
	ok = application:start(sporkk).

%% Starts the application's core supervisor.
start(_Type, _Args) ->
	% Initialize the database if necessary.
	sporkk_cfg:init(),
	% Start the main supervisor.
	case sporkk_sup:start_link() of
		{ok, Pid} ->
			{ok, Pid};
		Error ->
			Error
	end.

%% Called to stop the application.
stop(_State) ->
	exit(whereis(sporkk_sup), shutdown).


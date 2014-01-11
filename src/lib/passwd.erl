%% ============================================================================
%% @author Forkk <forkk@forkk.net> [http://forkk.net]
%% @copyright 2013 Forkk
%% @doc Utility functions for password hashing.
%% ============================================================================

-module(passwd).
-author("Forkk").

-export([
		 hash_pass/1, hash_pass/2, hash_pass/3,
		 check_pass/2
		]).

%% @doc Salts and hashes the given password with a random salt using sha512.
hash_pass(Pass) ->
	hash_pass(sha512, Pass).

%% @doc Salts and hashes the given password with a random 64 character salt using the given hashing algorithm.
hash_pass(Algorithm, Pass) ->
	hash_pass(Algorithm, Pass, crypto:rand_bytes(64)).

%% @doc Salts with the given salt and hashes the password.
hash_pass(Algorithm, Pass, Salt) ->
	{Algorithm, Salt, crypto:hash(Algorithm, Pass ++ Salt)}.


%% @doc Returns 'ok' if the given pass matches the given hash. Otherwise, returns 'fail'.
check_pass(Pass, {Alg, Salt, Hash}) ->
	{Alg, Salt, CheckHash} = hash_pass(Alg, Pass, Salt),
	case string:equal(Hash, CheckHash) of
		true -> ok;
		false -> fail
	end.


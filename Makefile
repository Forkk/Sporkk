all: 	deps compile

compile:
	rebar compile

deps:
	rebar get-deps

clean:
	rebar clean

distclean: clean devclean relclean
	rebar delete-deps

test:
	rebar skip_deps=true eunit


rel: all
	cd rel && rebar generate

relclean:
	rm -rf rel/erb.SUFFIXES: .erl .beam .yrl

run-shell:
	@erl -sname sporkk -pa ebin


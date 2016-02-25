.PHONY: deps precompile release

deps:
	rebar get-deps
	rebar prepare-deps

precompile:
	@(test -d deps || $(MAKE) deps)

release: precompile
	rebar clean
	rebar compile
	rebar escriptize

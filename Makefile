REBAR=`which rebar` || ./rebar
END2END=./tests/end2end.escript
all: deps compile
deps:
	@$(REBAR) get-deps
compile:
	@$(REBAR) compile
test:
	@$(REBAR) skip_deps=true eunit
clean:
	@$(REBAR) clean
e2e: compile
	@$(END2END)

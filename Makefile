
# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3)


all: $(REBAR3)
	@$(REBAR3) do clean, compile, eunit, ct, dialyzer


rel: all
	@$(REBAR3) release


clean: $(REBAR3)
	@$(REBAR3) clean
	rm -rf _build

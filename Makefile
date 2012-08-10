REBAR = ./rebar $(REBAR_ARGS)
REBAR_URL = http://cloud.github.com/downloads/Motiejus/rebar/rebar

.PHONY: clean compile test

compile: rebar
	$(REBAR) get-deps compile xref

clean:
	$(REBAR) clean

test: REBAR_ARGS = -C rebar_test.config
test: compile
	$(REBAR) eunit -v skip_deps=true

rebar:
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar

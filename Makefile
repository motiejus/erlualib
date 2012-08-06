REBAR_URL = http://cloud.github.com/downloads/Motiejus/rebar/rebar

REBAR_EXEC = $(shell which rebar || echo ./rebar)
REBAR = $(REBAR_EXEC) $(REBAR_ARGS)

.PHONY: clean compile test

compile: $(REBAR_EXEC)
	$(REBAR) get-deps compile

clean:
	$(REBAR) clean

test: REBAR_ARGS = -C rebar_test.config
test: compile
	$(REBAR) eunit -v skip_deps=true

$(REBAR_EXEC):
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar

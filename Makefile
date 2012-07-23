REBAR=./rebar
REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar

.PHONY: clean compile test

compile: rebar
	$(REBAR) get-deps compile

clean:
	$(REBAR) clean

test:
	$(REBAR) -C rebar_test.config get-deps compile
	$(REBAR) -C rebar_test.config eunit -v skip_deps=true

rebar:
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar

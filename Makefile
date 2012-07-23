REBAR=./rebar
REBAR_URL=http://cloud.github.com/downloads/basho/rebar/rebar

.PHONY: clean compile test

compile: $(REBAR)
	$(REBAR) get-deps compile

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit -v

$(REBAR):
	erl -noshell -s inets start -s ssl start \
        -eval 'httpc:request(get, {"$(REBAR_URL)", []}, [], [{stream, "./rebar"}])' \
        -s inets stop -s init stop
	chmod +x ./rebar

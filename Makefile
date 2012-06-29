REBAR=`which rebar`

.PHONY: clean compile test

compile:
	$(REBAR) compile

clean:
	$(REBAR) clean

test:
	$(REBAR) eunit -v

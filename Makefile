REBAR = `which rebar`

all: deps compile

compile: clean
	@( $(REBAR) compile )

clean:
	@( $(REBAR) clean )

run:
	@( erl -config app.config -pa ebin -s looker )

.PHONY: all deps compile clean run
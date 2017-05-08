ERL_LIBS = $(shell pwd)/lib

all:
	@cd lib/one; erlc -o ebin +debug_info src/*.erl
	@cd lib/two; erlc -o ebin +debug_info src/*.erl

clean:
	@rm -f lib/*/ebin/*.beam

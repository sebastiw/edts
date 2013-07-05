all:
	@cd lib/one; erlc -o ebin -pa `pwd`/ebin +debug_info src/*.erl
	@cd lib/two; erlc -o ebin -pa `pwd`/ebin +debug_info src/*.erl

clean:
	@rm -f lib/*/ebin/*.beam

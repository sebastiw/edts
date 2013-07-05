all:
	@cd lib/one; erlc -o ebin -pa `pwd`/ebin src/*.erl
	@cd lib/two; erlc -o ebin -pa `pwd`/ebin src/*.erl

clean:
	@rm lib/one/ebin/*.beam
	@rm lib/two/ebin/*.beam

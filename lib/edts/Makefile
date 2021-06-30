
all:
	mkdir -p ebin
	@cp src/edts.app.src ebin/edts.app
	@erlc +debug_info -pa ebin -o ebin src/edts_cmd.erl src/*.erl

clean:
	rm -rfv ebin

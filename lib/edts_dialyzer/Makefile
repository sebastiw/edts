
all:
	@mkdir -p ebin
	@cp src/edts_dialyzer.app.src ebin/edts_dialyzer.app
	@erlc +debug_info -pa ../edts/ebin -o ebin src/*.erl

clean:
	rm -rfv ebin

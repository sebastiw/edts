
all:
	@mkdir -p ebin
	@cp src/edts_debug.app.src ebin/edts_debug.app
	@erlc +debug_info -pa ../edts/ebin -o ebin src/*.erl

clean:
	rm -rfv ebin

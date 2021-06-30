
all:
	@mkdir -p ebin
	@cp src/edts_xref.app.src ebin/edts_xref.app
	@erlc +debug_info -pa ../edts/ebin -o ebin src/*.erl

clean:
	rm -rfv ebin

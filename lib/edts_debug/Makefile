
.PHONY: all compile test clean
all: compile

compile: ebin ebin/edts_debug.app
	@erlc +debug_info -pa ../edts/ebin -o ebin src/*.erl

test: ebin ebin/edts_debug.app
	@erlc +debug_info -DTEST -pa ../edts/ebin -o ebin src/*.erl

clean:
	@rm -rfv ebin


ebin:
	@mkdir -p ebin
ebin/edts_debug.app:
	@cp src/edts_debug.app.src ebin/edts_debug.app

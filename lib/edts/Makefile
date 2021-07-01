
.PHONY: all compile test clean

all: compile

compile: ebin ebin/edts.app
	@erlc +debug_info -pa ebin -o ebin src/edts_cmd.erl src/*.erl

test: ebin ebin/edts.app
	@erlc +debug_info -DTEST -pa ebin -o ebin src/edts_cmd.erl src/*.erl

clean:
	@rm -rfv ebin

ebin:
	@mkdir -p ebin

ebin/edts.app:
	@cp src/edts.app.src ebin/edts.app

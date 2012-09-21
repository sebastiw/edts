MAKEFLAGS=-s

EMACS=emacs -q --no-site-file

.PHONY: all
all: el erl

el:
	$(EMACS) --batch \
	        -l edts-start.el \
		-f batch-byte-compile $(wildcard ./elisp/*/*.el)
erl:
	@-if [ -z "${EDTS_SKIP_SUBMODULE_UPDATE}" ]; \
	  then git submodule update --init; fi
	@cd lib/edts && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)"

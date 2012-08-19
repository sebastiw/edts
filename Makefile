MAKEFLAGS=-s

.PHONY: all
all:
	@-if [ -z "${EDTS_SKIP_SUBMODULE_UPDATE}" ]; \
	  then git submodule update --init; fi
	@cd lib/webmachine && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)"
	@cd lib/edts && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)"

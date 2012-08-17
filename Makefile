MAKEFLAGS=-s

.PHONY: all
all:
	@cd lib/webmachine && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)"
	@cd lib/edts && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)"
	@cd lib/distel && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)"

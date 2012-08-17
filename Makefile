.PHONY: all
all:
	cd lib/webmachine && $(MAKE)
	cd lib/edts && $(MAKE)
	cd lib/distel && $(MAKE)

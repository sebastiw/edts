MAKEFLAGS = -s
PLUGINS = $(wildcard plugins/*)
ERL_LIBS=`pwd`"/lib"

.PHONY: all
all: submodule-update libs $(PLUGINS)

.PHONY: submodule-update
submodule-update:
	@-if [ -z "${EDTS_SKIP_SUBMODULE_UPDATE}" ]; \
	then git submodule update --init; fi

.PHONY: libs
libs:
	$(MAKE) -C lib/edts MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: plugins
plugins: $(PLUGINS)

.PHONY: $(PLUGINS)
$(PLUGINS):
	$(MAKE) -e ERL_LIBS="$(ERL_LIBS)" -C $@ MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: clean
clean: $(PLUGINS:%=clean-%)
	rm -rfv elisp/*/*.elc
	$(MAKE) -C test/edts-test-project1 MAKEFLAGS="$(MAKEFLAGS)" clean
	$(MAKE) -C lib/edts MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: $(PLUGINS:%=clean-%)
$(PLUGINS:%=clean-%):
	$(MAKE) -C $(@:clean-%=%) MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: ert
ert:
	$(MAKE) -C test/edts-test-project1 MAKEFLAGS="$(MAKEFLAGS)"
	emacs -q --no-splash --batch \
	--eval "(add-to-list 'load-path  \"${PWD}/elisp/ert\")" \
	-l edts-start.el \
	-f ert-run-tests-batch-and-exit

.PHONY: test
test: all ert test-edts $(PLUGINS:%=test-%)

:PHONY: test-edts
test-edts:
	$(MAKE) -C lib/edts MAKEFLAGS="$(MAKEFLAGS)" test

.PHONY: $(PLUGINS:%=test-%)
$(PLUGINS:%=test-%):
	$(MAKE) -e ERL_LIBS="$(ERL_LIBS)" -C $(@:test-%=%) MAKEFLAGS="$(MAKEFLAGS)" test

.PHONY: eunit
eunit: all eunit-edts $(PLUGINS:%=eunit-%)

:PHONY: eunit-edts
eunit-edts:
	$(MAKE) -C lib/edts MAKEFLAGS="$(MAKEFLAGS)" eunit

.PHONY: $(PLUGINS:%=eunit-%)
$(PLUGINS:%=eunit-%):
	$(MAKE) -e ERL_LIBS="$(ERL_LIBS)" -C $(@:eunit-%=%) MAKEFLAGS="$(MAKEFLAGS)" eunit


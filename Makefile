MAKEFLAGS = -s
PLUGINS = $(subst plugins/,,$(wildcard plugins/*))
export ERL_LIBS:=`pwd`"/lib"
EMACS?= "emacs"

.PHONY: all
all: submodule-update libs plugins

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
	$(MAKE) -e ERL_LIBS="$(ERL_LIBS)" -C plugins/$@ MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: clean
clean: $(PLUGINS:%=clean-%)
	rm -rfv elisp/*/*.elc
	$(MAKE) -C test/edts-test-project1 MAKEFLAGS="$(MAKEFLAGS)" clean
	$(MAKE) -C lib/edts MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: $(PLUGINS:%=clean-%)
$(PLUGINS:%=clean-%):
	$(MAKE) -C plugins/$(@:clean-%=%) MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: integration-tests
integration-tests:
	$(MAKE) -C test/edts-test-project1 MAKEFLAGS="$(MAKEFLAGS)"
	$(EMACS) -Q --batch \
	-L ${PWD} \
	-l test/load-tests.el \
	-f edts-test-run-suites-batch-and-exit

.PHONY: ert
ert:
	$(MAKE) -C test/edts-test-project1 MAKEFLAGS="$(MAKEFLAGS)"
	$(EMACS) -Q --batch \
	-L ${PWD} \
	-l test/load-tests.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag edts-test-suite)))"

.PHONY: test
test: libs plugins test-edts ert integration-tests $(PLUGINS:%=test-%)

:PHONY: test-edts
test-edts:
	$(MAKE) -C lib/edts MAKEFLAGS="$(MAKEFLAGS)" test

.PHONY: $(PLUGINS:%=test-%)
$(PLUGINS:%=test-%):
	$(MAKE) -e ERL_LIBS="$(ERL_LIBS)" -C plugins/$(@:test-%=%) MAKEFLAGS="$(MAKEFLAGS)" test

.PHONY: eunit
eunit: libs plugins eunit-edts $(PLUGINS:%=eunit-%)

:PHONY: eunit-edts
eunit-edts:
	$(MAKE) -C lib/edts MAKEFLAGS="$(MAKEFLAGS)" eunit

.PHONY: $(PLUGINS:%=eunit-%)
$(PLUGINS:%=eunit-%):
	$(MAKE) -e ERL_LIBS="$(ERL_LIBS)" -C plugins/$(@:eunit-%=%) MAKEFLAGS="$(MAKEFLAGS)" eunit


MAKEFLAGS=-s
PLUGINS= $(wildcard plugins/*)

.PHONY: all
all: $(PLUGINS)
	@-if [ -z "${EDTS_SKIP_SUBMODULE_UPDATE}" ]; \
	then git submodule update --init; fi
	@cd lib/edts && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: $(PLUGINS)
$(PLUGINS):
	@cd $@ && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: clean
clean: clean-$(PLUGINS)
	rm -rfv elisp/*/*.elc
	@cd lib/edts && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: $(SPLUGINS:%=clean-%)
clean-$(PLUGINS):
	@cd $(@:clean-%=%) && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: ert
ert:
	emacs -q --no-splash --batch \
	--eval "(add-to-list 'load-path  \"${PWD}/elisp/ert\")" \
	-l edts-start.el \
	-f ert-run-tests-batch-and-exit

.PHONY: eunit
eunit: eunit-$(PLUGINS)
	@(cd lib/edts; ./rebar eunit skip_deps=true)

.PHONY: $(SPLUGINS:%=eunit-%)
eunit-$(PLUGINS):
	@cd $(@:eunit-%=%) && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)" eunit

.PHONY: ct
ct: ct-$(PLUGINS)
	@(cd lib/edts; ./rebar ct skip_deps=true)

.PHONY: $(SPLUGINS:%=ct-%)
ct-$(PLUGINS):
	@cd $(@:eunit-%=%) && $(MAKE) MAKEFLAGS="$(MAKEFLAGS)" ct

.PHONY: test
test: all ert eunit ct

MAKEFLAGS = -s
PLUGINS = $(subst plugins/,,$(wildcard plugins/*))
export ERL_LIBS:=`pwd`"/lib"
EMACS?= "emacs"

REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

# If there is a rebar in the current directory, use it
ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

# Fallback to rebar on PATH
REBAR3 ?= $(shell which rebar3)

# And finally, prep to download rebar if all else fails
ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

$(REBAR3):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

.PHONY: all
all: submodule-update libs plugins

.PHONY: submodule-update
submodule-update:
	@-if [ -z "${EDTS_SKIP_SUBMODULE_UPDATE}" ]; \
	then git submodule update --init; fi

.PHONY: libs
libs: $(REBAR3)
	@$(REBAR3) release

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
	@$(REBAR3) do eunit --dir="apps/edts/src", ct

.PHONY: $(PLUGINS:%=test-%)
$(PLUGINS:%=test-%):
	$(MAKE) -e ERL_LIBS="$(ERL_LIBS)" -C plugins/$(@:test-%=%) MAKEFLAGS="$(MAKEFLAGS)" test

.PHONY: eunit
eunit: libs plugins eunit-edts $(PLUGINS:%=eunit-%)

:PHONY: eunit-edts
eunit-edts:
	@$(REBAR3) eunit --dir="apps/edts/src"
## The --dir flag is only here, because otherwise it sets it to
## "./test" by default, and we have other stuff there that breaks
## eunit

.PHONY: $(PLUGINS:%=eunit-%)
$(PLUGINS:%=eunit-%):
	$(MAKE) -e ERL_LIBS="$(ERL_LIBS)" -C plugins/$(@:eunit-%=%) MAKEFLAGS="$(MAKEFLAGS)" eunit

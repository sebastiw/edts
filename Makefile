MAKEFLAGS = -s
APPS = $(subst lib/,,$(wildcard lib/*))
EUNIT_FILES = $(subst $(empty) ,$(comma),$(wildcard lib/*/src/*.erl))
EMACS ?= "emacs"
DOCKER ?= "docker"
ERL_PATH ?= $(subst /bin/erl,,$(shell which erl))
ERLANG_EMACS_LIB ?= $(wildcard $(ERL_PATH)/lib/tools*/emacs)

REBAR3 ?= $(shell which rebar3)
ifeq (,$(REBAR3))
REBAR3 := $(CURDIR)/rebar3
endif

comma = ,

.PHONY: all
all: compile release

.PHONE: compile
compile: apps deps

.PHONY: apps
apps: $(APPS)
$(APPS):
	$(MAKE) -C lib/$@ MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: deps
deps:
	@mkdir -p deps
	@git clone https://github.com/mochi/mochiweb deps/mochiweb
	$(MAKE) -C deps/mochiweb MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: release
release:
	@mkdir -p rel
	./release-edts

.PHONY: clean
clean: $(APPS:%=clean-%)
	rm -rfv elisp/*/*.elc rel deps .dialyzer_plt

.PHONY: $(APPS:%=clean-%)
$(APPS:%=clean-%):
	$(MAKE) -C lib/$(@:clean-%=%) MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: dialyzer
dialyzer: .dialyzer_plt
	dialyzer --add_to_plt -r lib/ deps/

.dialyzer_plt:
	dialyzer --build_plt --apps erts kernel stdlib sasl dialyzer tools inets crypto debugger wx

.PHONY: test
test: apps-test dialyzer integration-tests ert

.PHONY: apps-test
apps-test: eunit

.PHONY: eunit $(EUNIT_FILES:%=eunit-%)
eunit: $(EUNIT_FILES:%=eunit-%)
$(EUNIT_FILES:%=eunit-%):
	# Need to update path to become module
	erl -noshell -run $(@:eunit-%=%) test -run init stop

.PHONY: $(APPS:%=test-%)
$(APPS:%=test-%): $(REBAR3)
	@echo "Using $(REBAR3)"
	@$(REBAR3) do eunit --dir "$(wildcard lib/*/src)", ct

.PHONY: integration-tests
integration-tests: all test-projects
	@echo "Erlang Emacs path: " $(ERLANG_EMACS_LIB)
	$(EMACS) -Q --batch \
	-L $(ERLANG_EMACS_LIB) \
	-l test_data/load-tests.el \
	--debug-init \
	-f edts-test-run-suites-batch-and-exit

.PHONY: ert
ert: test-projects
	@echo "Erlang Emacs path: " $(ERLANG_EMACS_LIB)
	$(EMACS) -Q --batch \
	-L $(ERLANG_EMACS_LIB) \
	-l test_data/load-tests.el \
	--debug-init \
	--eval "(ert-run-tests-batch-and-exit '(not (tag edts-test-suite)))"

.PHONY: byte-compilation-test
byte-compilation-test:
	$(DOCKER) run -it --rm \
	-v ${HOME}:${HOME} \
	-v /etc/passwd:/etc/passwd \
	-v /usr/lib/erlang:/usr/lib/erlang \
	-v /usr/bin:/usr/bin \
	-w ${PWD} \
	-e PATH=${PATH} \
	-u $(shell id -u) \
	silex/emacs \
	/bin/bash -c \
	'emacs -Q --batch -L ${PWD} -l test_data/package-install-deps.el \
	-f batch-byte-compile *.el elisp/edts/*.el lib/**/*.el'

.PHONY: test-projects
test-projects:
	$(MAKE) -C test_data/edts-test-project-project-1 MAKEFLAGS="$(MAKEFLAGS)"

MAKEFLAGS = -s
APPS = $(subst lib/,,$(wildcard lib/*))
EUNIT_FILES = $(wildcard lib/*/src/*.erl)
EMACS ?= "emacs"
DOCKER ?= "docker"
MKDIR ?= "mkdir"
MKDIR_FLAGS ?= "-p"
GIT ?= "git"
GIT_CMD ?= "clone"
ERL ?= "erl"
DIALYZER ?= "dialyzer"
ERL_PATH ?= $(subst /bin/erl,,$(shell which erl))
ERLANG_EMACS_LIB ?= $(wildcard $(ERL_PATH)/lib/tools*/emacs)

comma = ,

.PHONY: all
all: compile release

.PHONY: compile compile-test
compile: apps dependencies
compile-test: compile-apps-test

.PHONY: apps compile-apps-test
apps: $(APPS)
$(APPS):
	$(MAKE) -C lib/$@ MAKEFLAGS="$(MAKEFLAGS)"

compile-apps-test: $(APPS:%=testing-%)
$(APPS:%=testing-%):
	$(MAKE) -C lib/$(@:testing-%=%) MAKEFLAGS="$(MAKEFLAGS)" test

.PHONY: dependencies dependencies-test
dependencies: | deps/mochiweb
dependencies-test: | deps/mochiweb deps/meck

deps:
	$(MKDIR) $(MKDIR_FLAGS) deps
deps/mochiweb: | deps
	$(GIT) $(GIT_CMD) "https://github.com/mochi/mochiweb" deps/mochiweb
	$(MAKE) -C deps/mochiweb MAKEFLAGS="$(MAKEFLAGS)"
deps/meck: | deps
	$(GIT) $(GIT_CMD) "https://github.com/eproxus/meck" deps/meck
	$(MKDIR) $(MKDIR_FLAGS) deps/meck/ebin
	@erlc -o deps/meck/ebin deps/meck/src/*.erl

.PHONY: release
release:
	$(MKDIR) $(MKDIR_FLAGS) rel
	./edts-escript release

.PHONY: clean
clean: $(APPS:%=clean-%)
	rm -rfv elisp/*/*.elc rel deps .dialyzer_plt

.PHONY: $(APPS:%=clean-%)
$(APPS:%=clean-%):
	$(MAKE) -C lib/$(@:clean-%=%) MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: dialyzer
dialyzer: .dialyzer_plt
	$(DIALYZER) --add_to_plt -r lib/ deps/

.dialyzer_plt:
	$(DIALYZER) --build_plt --apps erts kernel stdlib sasl dialyzer tools inets crypto debugger wx

.PHONY: test
test: compile-apps-test dialyzer integration-tests ert

.PHONY: eunit $(EUNIT_FILES:%=eunit-%)
eunit: compile-test $(EUNIT_FILES:%=eunit-%)
$(EUNIT_FILES:%=eunit-%):
	$(foreach eunit_file,$(@:eunit-%=%),./edts-escript eunit $(eunit_file) deps/**/ebin)

.PHONY: integration-tests
integration-tests: all test-projects
	echo "$(EMACS) -Q --batch -L $(ERLANG_EMACS_LIB) -l test_data/load-tests.el --debug-init"
	$(EMACS) -Q --batch \
	-L $(ERLANG_EMACS_LIB) \
	-l test_data/load-tests.el \
	--debug-init \
	-f edts-test-run-suites-batch-and-exit

.PHONY: ert
ert: test-projects
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

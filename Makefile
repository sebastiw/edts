MAKEFLAGS = -s
APPS = $(subst lib/,,$(wildcard lib/*))
EUNIT_DIRS = $(subst $(empty) ,$(comma),$(wildcard lib/*/src))
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

$(REBAR3):
	curl -LO https://s3.amazonaws.com/rebar3/rebar3
	chmod a+x rebar3

.PHONY: compile
compile: $(REBAR3)
	@echo "Using $(REBAR3)"
	@$(REBAR3) compile

.PHONY: release
release: $(REBAR3)
	@echo "Using $(REBAR3)"
	@$(REBAR3) release

.PHONY: clean
clean: $(REBAR3)
	@echo "Using $(REBAR3)"
	@$(REBAR3) clean
	rm -rfv elisp/*/*.elc

.PHONY: dialyzer
dialyzer: $(REBAR3)
	@echo "Using $(REBAR3)"
	@$(REBAR3) dialyzer

.PHONY: test
test: apps-test dialyzer integration-tests ert

.PHONY: apps-test
apps-test: $(REBAR3)
	@echo "Using $(REBAR3)"
	@$(REBAR3) do eunit --dir="$(EUNIT_DIRS)", ct

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

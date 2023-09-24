# MAKEFLAGS = -s
LIBS = lib/edts lib/edts_debug lib/edts_dialyzer lib/edts_xref
DEPS = mochiweb meck
MKDIR ?= mkdir
MKDIR_FLAGS ?= "-p"
GIT ?= git

# For Testing
ERL ?= erl
ERLC ?= erlc
EMACS ?= emacs
DIALYZER ?= dialyzer
DOCKER ?= docker
ERL_PATH ?= $(subst /bin/erl,,$(shell which $(ERL)))
export ERLANG_EMACS_LIB ?= $(wildcard $(ERL_PATH)/lib/tools*/emacs)
OTP_TESTS = $(patsubst test_data/manual/Dockerfile.%,%,$(wildcard test_data/manual/Dockerfile.*))

all: compile release

.PHONY: compile
compile: $(LIBS) | deps/mochiweb

.PHONY: release
release: rel/releases
	./edts-escript release edts-release.config

rel/releases:
	$(MKDIR) $(MKDIR_FLAGS) rel/releases

deps/mochiweb/src:
	$(GIT) clone "https://github.com/mochi/mochiweb" deps/mochiweb
	cd deps/mochiweb && $(GIT) checkout --detach 835b107a4da4550080d032623ff6ae9a18d02c37
deps/meck/src:
	$(GIT) clone "https://github.com/eproxus/meck" deps/meck
	cd deps/meck && $(GIT) checkout --detach cc47aab4b64a46a5409c1a93353d44a367b41454

deps/mochiweb: deps/mochiweb/src deps/mochiweb/ebin
deps/meck: deps/meck/src deps/meck/ebin

$(DEPS:%=deps/%/ebin):
	$(MKDIR) $(MKDIR_FLAGS) $@
	$(ERLC) +debug_info -o $@ -I$(@:/ebin=/include) -I$(@:/ebin=/src) $(@:/ebin=/src)/*.erl
	# reltool doesn't support unrecognized options
	sed -i 'H; $$!d; x; s/[[:space:]]\+%[^\n]\+//g; s/,[[:space:]]\+{licenses[^\n]\+//; s/{links, \[[^]]\+]}//;' $(@:/ebin=/src)/$(@:deps/%/ebin=%).app.src
	cp $(@:/ebin=/src)/$(@:deps/%/ebin=%).app.src $@/$(@:deps/%/ebin=%).app

.PHONY: $(LIBS)
$(LIBS):
	$(MAKE) -C $@ MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: eunit $(LIBS:%=test-%)
eunit: $(LIBS:%=test-%)
$(LIBS:%=test-%): deps/meck
	$(MAKE) -C $(@:test-%=%) ERLC_EXTRA_PATHS="$(realpath deps/mochiweb/ebin) $(realpath deps/meck/ebin)" MAKEFLAGS="$(MAKEFLAGS)" test

.PHONY: clean
clean: $(LIBS:%=clean-%)
	rm -rfv *.elc elisp/*/*.elc rel deps edts.plt erlang.plt

.PHONY: $(LIBS:%=clean-%)
$(LIBS:%=clean-%):
	$(MAKE) -C $(@:clean-%=%) MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: dialyzer
dialyzer: erlang.plt edts.plt
	$(DIALYZER) --get_warnings -pa deps/meck/ebin --plt edts.plt -r lib/edts

erlang.plt:
	$(DIALYZER) --quiet --build_plt --output_plt $@ --apps \
		erts kernel stdlib mnesia crypto sasl eunit \
		syntax_tools compiler tools debugger dialyzer \
		wx runtime_tools
edts.plt: erlang.plt
	$(DIALYZER) --add_to_plt --plt erlang.plt -r lib/ deps/ --output_plt $@

.PHONY: test manual-tests
test: eunit dialyzer integration-tests ert
manual-tests: $(OTP_TESTS) byte-compilation-test

.PHONY: integration-tests
integration-tests: all test-projects
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

.PHONY: test-projects
test-projects:
	$(MAKE) -C test_data/edts-test-project-project-1 MAKEFLAGS="$(MAKEFLAGS)"

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

.PHONY: $(OTP_TESTS)
$(OTP_TESTS):
	$(DOCKER) build \
	--ulimit nofile=4096 \
	-f test_data/manual/Dockerfile.$@ .


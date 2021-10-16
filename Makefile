# MAKEFLAGS = -s
LIBS = lib/edts lib/edts_debug lib/edts_dialyzer lib/edts_xref
MKDIR ?= mkdir
MKDIR_FLAGS ?= "-p"
GIT ?= git
GIT_CMD ?= clone

# For Testing
ERL ?= erl
EMACS ?= emacs
DIALYZER ?= dialyzer
DOCKER ?= docker
ERL_PATH ?= $(subst /bin/erl,,$(shell which $(ERL)))
export ERLANG_EMACS_LIB ?= $(wildcard $(ERL_PATH)/lib/tools*/emacs)

all: compile release

.PHONY: compile
compile: $(LIBS) | deps/mochiweb

.PHONY: release
release: rel/releases
	./edts-escript release edts-release.config

rel/releases:
	$(MKDIR) $(MKDIR_FLAGS) rel/releases

deps/mochiweb:
	$(GIT) $(GIT_CMD) "https://github.com/mochi/mochiweb" $@
	# reltool doesn't support unrecognized options
	sed -i '/applications/{N; s/,$$// };/licenses/d;/links/d' $@/src/mochiweb.app.src
	$(MAKE) -C $@ MAKEFLAGS="$(MAKEFLAGS)"
deps/meck:
	$(GIT) $(GIT_CMD) "https://github.com/eproxus/meck" $@
	$(MKDIR) $(MKDIR_FLAGS) $@/ebin
	@erlc +debug_info -o $@/ebin -I$@/include -I$@/src $@/src/*.erl
	sed -i '/env/{ s/,$$// };/licenses/d;/links/{ N; N; N; d }' $@/src/meck.app.src
	cp $@/src/meck.app.src $@/ebin/meck.app

.PHONY: $(LIBS)
$(LIBS):
	$(MAKE) -C $@ MAKEFLAGS="$(MAKEFLAGS)"

.PHONY: eunit $(LIBS:%=test-%)
eunit: $(LIBS:%=test-%)
$(LIBS:%=test-%): deps/meck
	$(MAKE) -C $(@:test-%=%) ERLC_EXTRA_PATHS="$(realpath deps/mochiweb/ebin) $(realpath deps/meck/ebin)" MAKEFLAGS="$(MAKEFLAGS)" test

.PHONY: clean
clean: $(LIBS:%=clean-%)
	rm -rfv elisp/*/*.elc rel deps edts.plt erlang.plt

.PHONY: $(LIBS:%=clean-%)
$(LIBS:%=clean-%):
	$(MAKE) -C $(@:clean-%=%) MAKEFLAGS="$(MAKEFLAGS)" clean

.PHONY: dialyzer
dialyzer: erlang.plt edts.plt
	$(DIALYZER) --get_warnings -pa deps/meck/ebin --plt edts.plt -r lib/edts

erlang.plt:
	$(DIALYZER) --quiet --build_plt --output_plt $@ --apps \
		erts kernel stdlib mnesia crypto sasl eunit \
		syntax_tools compiler tools debugger dialyzer
edts.plt: erlang.plt
	$(DIALYZER) --add_to_plt --plt erlang.plt -r lib/ deps/ --output_plt $@

.PHONY: test
test: eunit dialyzer integration-tests ert

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

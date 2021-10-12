ERLC := erlc
ERLC_OPTIONS ?= $(if $(filter test,$(MAKECMDGOALS)),+debug_info -DTEST,+debug_info)
ERLC_EXTRA_PATHS ?=
ERLC_PATHS := $(realpath ../edts/ebin) ebin $(ERLC_EXTRA_PATHS)

OUT_DIR := ebin

ERL_SOURCES := $(wildcard src/*.erl)
APP_SOURCES := $(wildcard src/*.app.src)
APP_TARGET := $(patsubst src/%.app.src,$(OUT_DIR)/%.app,$(APP_SOURCES))
BEAM_FIRST_TARGETS := ebin/edts_event.beam ebin/edts_plugins.beam ebin/edts_events.beam ebin/edts_cmd.beam
BEAM_TARGETS := $(filter-out $(BEAM_FIRST_TARGETS), $(patsubst src/%.erl,$(OUT_DIR)/%.beam,$(ERL_SOURCES)))
TEST_TARGETS := $(wildcard $(OUT_DIR)/*.beam)
VPATH = src test

all: $(BEAM_FIRST_TARGETS) | $(BEAM_TARGETS) $(APP_TARGET)

.PHONY: test clean
test: | all $(TEST_TARGETS) $(TEST_TARGETS:%=eunit-%)

$(TEST_TARGETS:%=eunit-%):
	$(foreach test_file,$(realpath $(@:eunit-%=%)),../../edts-escript eunit $(test_file) $(ERLC_PATHS))
clean:
	rm -rfv ebin

$(BEAM_FIRST_TARGETS): | $(OUT_DIR)
$(BEAM_TARGETS): | $(OUT_DIR)
$(APP_TARGET): | $(OUT_DIR)

$(OUT_DIR)/%.app: src/%.app.src
	cp $< $@
$(OUT_DIR)/%.beam: %.erl
	$(ERLC) $(ERLC_OPTIONS) $(foreach path,$(ERLC_PATHS),-pa $(path)) -o $(OUT_DIR) $<

$(OUT_DIR):
	mkdir -p ebin

ERLC := erlc
ERLC_OPTIONS ?= $(if $(filter test,$(MAKECMDGOALS)),+debug_info -DTEST,+debug_info)
ERLC_EXTRA_PATHS ?=
ERLC_PATHS := $(realpath ../edts/ebin) ebin $(ERLC_EXTRA_PATHS)

OUT_DIR := ebin

ERL_SOURCES := $(wildcard src/*.erl)
APP_SOURCES := $(wildcard src/*.app.src)
APP_TARGET := $(patsubst src/%.app.src,$(OUT_DIR)/%.app,$(APP_SOURCES))
BEAM_FIRST_TARGETS :=
BEAM_TARGETS := $(filter-out $(BEAM_FIRST_TARGETS), $(patsubst src/%.erl,$(OUT_DIR)/%.beam,$(ERL_SOURCES)))
TEST_TARGETS := $(OUT_DIR)

all: $(BEAM_FIRST_TARGETS) | $(BEAM_TARGETS) $(APP_TARGET)

.PHONY: test clean
test: all $(TEST_TARGETS:%=eunit-%)

$(TEST_TARGETS:%=eunit-%):
	../../edts-escript eunit $(realpath $(@:eunit-%=%)) $(ERLC_PATHS)

clean:
	rm -rfv ebin

$(BEAM_TARGETS): | $(OUT_DIR)
$(APP_TARGET): | $(OUT_DIR)

$(OUT_DIR)/%.app: src/%.app.src
	cp $< $@
$(OUT_DIR)/%.beam: src/%.erl
	$(ERLC) $(ERLC_OPTIONS) -I ../edts/include $(foreach path,$(subst :, ,$(ERLC_PATHS)),-pa $(path)) -o $(OUT_DIR) $<

$(OUT_DIR):
	mkdir -p ebin

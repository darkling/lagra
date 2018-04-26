PROJECT = lagra
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
TEST_DIR = ct

include erlang.mk

.PHONY: parser-tests
parser-tests: $(subst _data,.erl,$(wildcard ct/parser_*_SUITE_data))

ct/parser_%_SUITE.erl: ct/parser_%_SUITE_data/manifest.ttl scripts/gen-parser-test-suite.sh
	scripts/gen-parser-test-suite.sh $< $(notdir $(subst .erl,,$@)) >$@

PROJECT = lagra
PROJECT_DESCRIPTION = RDF library for Erlang
PROJECT_VERSION = 0.1.0
TEST_DIR = ct

include erlang.mk

.PHONY: test-suites
test-suites: $(subst _data,.erl,$(wildcard ct/parser_*_SUITE_data)) \
			 $(subst _data,.erl,$(wildcard ct/serializer_*_SUITE_data)) \
             ct/isomorphism_SUITE.erl

ct/parser_%_SUITE.erl: ct/parser_%_SUITE_data/manifest.ttl scripts/gen-parser-test-suite.sh
	scripts/gen-parser-test-suite.sh $< $(notdir $(subst .erl,,$@)) >$@

ct/serializer_%_SUITE.erl: ct/serializer_%_SUITE_data/manifest.ttl scripts/gen-serializer-test-suite.sh
	scripts/gen-serializer-test-suite.sh $< $(notdir $(subst .erl,,$@)) >$@

ct/isomorphism_SUITE.erl: ct/isomorphism_SUITE_data/manifest.ttl scripts/gen-parser-test-suite.sh
	scripts/gen-parser-test-suite.sh $< $(notdir $(subst .erl,,$@)) >$@

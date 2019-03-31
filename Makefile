PROJECT = lagra
PROJECT_DESCRIPTION = RDF library for Erlang
PROJECT_VERSION = 0.1.0
TEST_DIR = ct

include erlang.mk

.PHONY: test-suites
test-suites: $(subst _data,.erl,$(wildcard ct/parser_*_SUITE_data)) \
			 $(subst _data,.erl,$(wildcard ct/serializer_*_SUITE_data)) \
             ct/isomorphism_SUITE.erl

ct/parser_ntriples_w3_SUITE.erl: ct/parser_ntriples_w3_SUITE_data/manifest.ttl scripts/gen-parser-test-suite.sh
	@echo " GENCT " $<
	$(Q)@scripts/gen-parser-test-suite.sh $< $(notdir $(subst .erl,,$@)) \
		"http://www.w3.org/2013/N-TriplesTests/manifest.ttl\#" >$@

ct/parser_turtle_w3_SUITE.erl: ct/parser_turtle_w3_SUITE_data/manifest.ttl scripts/gen-parser-test-suite.sh
	@echo " GENCT " $<
	$(Q)@scripts/gen-parser-test-suite.sh $< $(notdir $(subst .erl,,$@)) \
		"http://www.w3.org/2013/TurtleTests/manifest.ttl\#" >$@

ct/serializer_%_SUITE.erl: ct/serializer_%_SUITE_data/manifest.ttl scripts/gen-serializer-test-suite.sh
	@echo " GENCT " $<
	$(Q)@scripts/gen-serializer-test-suite.sh $< $(notdir $(subst .erl,,$@)) >$@

ct/isomorphism_SUITE.erl: ct/isomorphism_SUITE_data/manifest.ttl scripts/gen-parser-test-suite.sh
	@echo " GENCT " $<
	$(Q)@scripts/gen-parser-test-suite.sh $< $(notdir $(subst .erl,,$@)) "" >$@

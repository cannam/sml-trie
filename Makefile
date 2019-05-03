
SCRIPTS	:= ../sml-buildscripts

test:	test.mlb test.deps
	${SCRIPTS}/smlrun test.mlb
#	./test

test.deps: test.mlb trie.mlb
	${SCRIPTS}/mlb-dependencies $^ > $@

clean:
	rm -f test *.deps

.PHONY:	doc
doc:	
	mkdir -p doc
	$(SCRIPTS)/mlb-expand trie.mlb | grep -v '^ext' > .docfiles
	smldoc --nowarn -d doc -a .docfiles

-include *.deps

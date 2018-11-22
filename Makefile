
SCRIPTS	:= ../sml-buildscripts

test:	test.mlb test.deps
	${SCRIPTS}/polybuild test.mlb
	./test

test.deps: test.mlb trie.mlb
	${SCRIPTS}/mlb-dependencies $< > $@

clean:
	rm -f test *.deps

-include *.deps

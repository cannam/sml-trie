
test:	test.mlb
	mlton test.mlb
	./test

clean:
	rm -f test

test: trie.sig pattern-match-trie.sig
test: list-entry-trie-fn.sml string-trie.sml
test: tests.sig test-support.sml test-trie.sml

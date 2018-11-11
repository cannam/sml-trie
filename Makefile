
SCRIPTS	:= ../sml-buildscripts

test:	test.mlb test.deps
	${SCRIPTS}/polybuild test.mlb
	./test

test.deps: test.mlb
	${SCRIPTS}/mlb-dependencies $< > $@

clean:
	rm -f test

-include *.deps

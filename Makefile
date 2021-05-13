install:
	racket main.rkt

testmacros:
	raco test test/macros

test:
	raco test test
.PHONY: test

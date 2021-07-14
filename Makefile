# Test directories
Macro_dir=test/macros
Fsm_dir=test/fsm
Viz_dir=test/FSM-Visualization


install:
	@racket main.rkt
.PHONY: install

testmacros:
	@raco test $(Macro_dir)
.PHONY: testmacros

testviz:
	@raco test $(Viz_dir)
.PHONY: testviz

testfsm:
	@raco test $(Fsm_dir)
.PHONY: testfsm

test:
	@raco test test
.PHONY: test

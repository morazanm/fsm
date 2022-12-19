# Test directories
Fsm_dir=test/fsm
Viz_dir=test/FSM-Visualization


install:
	@racket main.rkt
.PHONY: install

test:
	@raco test fsm-test
.PHONY: test

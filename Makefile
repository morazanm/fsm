install:
	@raco make main.rkt
.PHONY: install

test:
	@raco test fsm-test fsm-gui fsm-core fsm-gviz
.PHONY: test

build-doc:
	@scribble fsm-doc/fsm.scrbl
.PHONY: build-doc

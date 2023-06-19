SHELL := /bin/bash

install:
	@raco make main.rkt
.PHONY: install

test:
	@raco test fsm-test fsm-gui fsm-core fsm-gviz
.PHONY: test

docs:
	cd fsm-docs; scribble +m ./fsm.scrbl; scribble +m ./dev.scrbl
.PHONY: docs

clean:
	rm -vf fsm-docs/*.{html,js,png,css}

rebuild:
	find . -type d -name compiled | xargs rm -rf
.PHONY: clean

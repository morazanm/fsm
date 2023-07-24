SHELL := /bin/bash

install:
	@raco make main.rkt
.PHONY: install

test:
	@raco test -t -j 4 fsm-test fsm-gui fsm-core fsm-gviz
.PHONY: test

docs:
	cd fsm-docs; scribble +m ./fsm.scrbl; scribble +m ./dev.scrbl
.PHONY: docs

clean:
	find . -type d -name compiled | xargs rm -rfv
	rm -vf fsm-docs/*.{html,js,png,css}

reinstall:
	@raco pkg remove fsm
	@raco pkg install
.PHONY: clean

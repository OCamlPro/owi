
OCAML_VERSION:=5.1.1

# You can override this version locally in Makefile.config
-include Makefile.config

help:
	@echo Use one of these targets:
	@echo
	@echo make deps : install dependencies
	@echo make owi : build only owi
	@echo make install : install owi
	@echo make dev-deps : install dev dependencies
	@echo make all : build everything
	@echo make test : run the tests
	@echo make test-promte : run the tests and promote failed tests
	@echo

owi:
	opam exec -- dune build -p owi @install

install: owi
	opam exec -- dune install

all:
	opam exec -- dune build

deps:
	test -e _opam || opam switch link $(OCAML_VERSION) || (opam switch create $(OCAML_VERSION) && opam switch link $(OCAML_VERSION))
	opam install --deps-only .

dev-deps:
	test -e _opam || opam switch link $(OCAML_VERSION) || (opam switch create $(OCAML_VERSION) && opam switch link $(OCAML_VERSION))
	opam install --deps-only --with-test --with-doc .

test:
	opam exec -- dune test

test-promote:
	opam exec -- dune test || opam exec -- dune promote

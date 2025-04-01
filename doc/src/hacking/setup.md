# Development Setup

To get a proper development setup:

```shell-session
$ git clone git@github.com:OCamlPro/owi.git
$ cd owi
$ opam install . --deps-only --with-test --with-doc --with-dev-setup
$ git submodule update --init --recursive
$ dune build @all
```

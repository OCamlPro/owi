# Hacking on Owi

## Development setup

To get a proper development setup:

```shell-session
$ git clone git@github.com:OCamlPro/owi.git
$ cd owi
$ opam install . --deps-only --with-test --with-doc --with-dev-setup
$ git submodule update --init --recursive
$ dune build @all
```

## Coding Guidelines

### The `prelude` library

We use the [`prelude`](https://forge.kumikode.org/kumikode/prelude) library to **hide dangerous functions** from the standard library.
It is automatically opened in the whole project.
More than dangerous functions, this library also hide some modules for which better alternatives exists.
For instance, all system interactions are done using [`Bos`](https://erratique.ch/software/bos/doc/) and all the formatting is done with [`Fmt`](https://erratique.ch/software/fmt/doc/).

### Printing

Read the [Logs basics](https://erratique.ch/software/logs/doc/Logs/index.html#basics) and in particular, the [usage conventions](https://erratique.ch/software/logs/doc/Logs/index.html#usage).

## Documentation

### API

You can build the documentation with:

```shell-session
$ dune build @doc
$ xdg-open _build/default/doc/index.html
```

### User Manual

```shell-session
$ cd ./doc
$ mdbook serve
```

## Testing

### Unit tests

Tests are mostly written using [Cram Tests].
The ones that are integrated into documentation are using [MDX].
You can run them as follow:

```shell-session
$ dune runtest
```

If you made some changes and the output of some tests is changing, the diff will be displayed.
If you want to automatically accept the diff as being the new expected output, you can run:

```shell-session
$ dune promote
```

### Code coverage

You can generate the code coverage report with:

```shell-session
BISECT_FILE=$(pwd)/bisect dune runtest --force --instrument-with bisect_ppx
bisect-ppx-report html -o _coverage
xdg-open _coverage/index.html
```

### Fuzzing

See [test/fuzz].

[Cram Tests]: https://dune.readthedocs.io/en/latest/reference/cram.html
[MDX]: https://github.com/realworldocaml/mdx
[test/fuzz]: ./test/fuzz 

## Diagnostic

### Profiling

#### Landmarks

```shell-session
OCAML_LANDMARKS=on dune exec --instrument-with landmarks --profile release -- owi run test/cram/run/binary_loop.wasm
```

See the discussion in [#871] to understand landmarks limitations in Owi.

### Benchmarking

#### Test-comp

See [Symbocalypse].

[#871]: https://github.com/OCamlPro/owi/pull/871
[Symbocalypse]: https://github.com/OCamlPro/symbocalypse


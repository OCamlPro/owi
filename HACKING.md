# Hacking on Owi

## Development set-up

To get a proper development setup:

```shell-session
$ git clone git@github.com:OCamlPro/owi.git
$ cd owi
$ opam install . --deps-only --with-test --with-doc --with-dev-setup
$ git submodule update --init --recursive
$ dune build @all
```

## Coding guidelines

### The `prelude` library

We use the [`prelude`](https://git.zapashcanon.fr/zapashcanon/prelude) library to **hide dangerous functions** from the standard library.
It is automatically opened in the whole project.
More than dangerous functions, this library also hide some modules for which better alternatives exists.
For instance, all system interactions are done using [`Bos`](https://erratique.ch/software/bos/doc/) and all the formatting is done with [`Fmt`](https://erratique.ch/software/fmt/doc/).

### Printing

Read the [Logs basics](https://erratique.ch/software/logs/doc/Logs/index.html#basics) and in particular, the [usage conventions](https://erratique.ch/software/logs/doc/Logs/index.html#usage).

## Testing

#### Unit tests

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

#### Fuzzing

See [test/fuzz].

## Benchmarking

### Landmarks

```shell-session
OCAML_LANDMARKS=on dune exec --instrument-with landmarks --profile release -- owi run test/run/binary_loop.wasm
```

Note: it seems landmarks is not compatible with domains and thus won't work with most sub-commands.

### Test-comp

See [bench].

[bench]: ./bench/README.md
[test/fuzz]: ./test/fuzz/README.md

[Cram Tests]: https://dune.readthedocs.io/en/latest/reference/cram.html
[MDX]: https://github.com/realworldocaml/mdx

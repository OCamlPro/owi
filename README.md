# owi

[owi] is an [OCaml] toolchain to work with WebAssembly. It provides an interpreter as an executable and a library.

## Installation

`owi` can be installed with [opam]:

```shell-session
$ opam install owi
```

If you don't have `opam`, you can install it following the [how to install opam] guide.

If you can't or don't want to use `opam`, consult the [opam file] for build instructions.

## Quickstart

Running the executable interpreter on a given Wasm file is as simple as:
```sh
$ cat test/passing/quickstart.wast
(module $quickstart
  (func $f
    i32.const 24
    i32.const 24
    i32.add
    drop
  )
  (start $f)
)
$ dune exec src/bin/owi.exe -- --debug test/passing/quickstart.wast
simplifying  ...
typechecking ...
linking      ...
interpreting ...
stack        : [  ]
running instr: call 0
calling func : func f
stack        : [  ]
running instr: i32.const 24
stack        : [ i32.const 24 ]
running instr: i32.const 24
stack        : [ i32.const 24 ; i32.const 24 ]
running instr: i32.add
stack        : [ i32.const 48 ]
running instr: drop
stack        : [  ]
stack        : [  ]
```

You can also pass the `--script` flag to run the file as a [reference test suite script]. This will allow you to add constructs like assertions and will also link the `spectest` module, which provides function for e.g. printing.

For more, have a look at the [example] folder, at the [documentation] or at the [test suite].

## About

- [LICENSE]
- [CHANGELOG]

[CHANGELOG]: ./CHANGES.md
[example]: ./example/
[LICENSE]: ./LICENSE.md
[opam file]: ./owi.opam
[test suite]: ./test/

[documentation]: https://ocamlpro.github.io/owi/api/owi
[how to install opam]: https://opam.ocaml.org/doc/Install.html
[OCaml]: https://ocaml.org
[opam]: https://opam.ocaml.org
[owi]: https://ocamlpro.github.io/owi
[reference test suite script]: https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#scripts
[WebAssembly]: https://webassembly.org

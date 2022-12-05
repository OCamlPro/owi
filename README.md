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

Given a file `test/passing/quickstart.wast` with the following content:

<!-- $MDX file=test/passing/quickstart.wast -->
```wast
(module $quickstart
  (func $f
    i32.const 24
    i32.const 24
    i32.add
    drop
  )
  (start $f)
)
```

Running the executable interpreter is as simple as:
```sh
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

You can also pass the `--script` flag to run the file as a [reference test suite script]. This will allow you to add constructs like assertions and will also link the [spectest module], which provides function for e.g. printing.

If you're interested in the library part of owi, here's how to get started:

```ocaml
# open Owi;;
# let filename = "test/passing/quickstart.wast";;
val filename : string = "test/passing/quickstart.wast"
# let script =
    match Parse.from_file ~filename with
    | Ok script -> script
    | Error e -> failwith e;;
val script : Types.file =
...
# let m =
    match script with
    | Module m :: _ -> m
    | _ -> failwith "expected a module first";;
val m : Types.module_ =
...
# let module_to_run, link_state =
    match Compile.until_link Link.empty_state m with
    | Ok v -> v
    | Error e -> failwith e;;
val module_to_run : Link.module_to_run =
...
val link_state : Link.state =
...
# let () =
    Log.debug_on := true;
    match Interpret.module_ module_to_run with
    | Ok () -> ()
    | Error e -> failwith e;;
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
[spectest module]: https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#spectest-host-module
[WebAssembly]: https://webassembly.org

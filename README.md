# [🐌] owi [![build status](https://github.com/ocamlpro/owi/workflows/build/badge.svg)](https://github.com/ocamlpro/owi/actions) [![coverage percentage](https://raw.githubusercontent.com/ocamlpro/owi/gh-pages/coverage/badge.svg)](https://ocamlpro.github.io/owi/coverage/)

[owi] is an [OCaml] toolchain to work with WebAssembly. It provides an interpreter as an executable and a library.

## Installation

`owi` can be installed with [opam]:

```shell-session
$ opam install owi
```

If you don't have `opam`, you can install it following the [how to install opam] guide.

If you can't or don't want to use `opam`, you can build the package with `dune build -p owi @install` but you'll first have to install the dependencies by yourself. You can find the list of dependencies in the [dune-project] file.

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
$ dune exec owi -- --debug test/passing/quickstart.wast
parsing      ...
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
# let m =
    match Parse.module_from_file ~filename with
    | Ok script -> script
    | Error e -> failwith e;;
val m : Symbolic.modul =
...
# let module_to_run, link_state =
    match Compile.until_link Link.empty_state ~optimize:false ~name:None m with
    | Ok v -> v
    | Error e -> failwith e;;
val module_to_run : Link.module_to_run =
...
val link_state : Link.state =
...
# let () =
    Log.debug_on := true;
    match Interpret.modul module_to_run with
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

## Supported proposals

The 🐌 status means the proposal is not applicable to owi.

### Adopted proposals

| Proposal                                           | Status  |
| -------------------------------------------------- | ------- |
| [Import/Export of Mutable Globals]                 | ✔️       |
| [Non-trapping float-to-int conversions]            | ✔️       |
| [Sign-extension operators]                         | ✔️       |
| [Multi-value]                                      | ✔️       |
| [Reference Types]                                  | ✔️       |
| [Bulk memory operations]                           | ✔️       |
| [Fixed-width SIMD]                                 | ❌      |
| [JavaScript BigInt to WebAssembly i64 integration] | 🐌      |

### Current proposals

We only list proposals that reached phase 3 at least.

| Proposal                                         | Status   |
| ------------------------------------------------ | -------- |
| [Tail call]                                      |  ✔️       |
| [Typed Function References]                      |  ✔️       |
| [Garbage collection]                             |  Ongoing |
| [Extended Constant Expressions]                  |  ❌      |
| [Multiple memories]                              |  ❌      |
| [Custom Annotation Syntax in the Text Format]    |  ❌      |
| [Memory64]                                       |  ❌      |
| [Exception handling]                             |  ❌      |
| [Branch Hinting]                                 |  ❌      |
| [Relaxed SIMD]                                   |  ❌      |
| [Threads]                                        |  ❌      |
| [Web Content Security Policy]                    |  🐌      |
| [JS Promise Integration]                         |  🐌      |
| [Type Reflection for WebAssembly JavaScript API] |  🐌      |

## About

- [LICENSE]
- [CHANGELOG]

[CHANGELOG]: ./CHANGES.md
[dune-project]: ./dune-project
[example]: ./example/
[LICENSE]: ./LICENSE.md
[test suite]: ./test/

[documentation]: https://ocamlpro.github.io/owi/api/owi
[how to install opam]: https://opam.ocaml.org/doc/Install.html
[OCaml]: https://ocaml.org
[opam]: https://opam.ocaml.org
[owi]: https://ocamlpro.github.io/owi
[reference test suite script]: https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#scripts
[spectest module]: https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#spectest-host-module
[WebAssembly]: https://webassembly.org

[Import/Export of Mutable Globals]: https://github.com/WebAssembly/mutable-global
[Non-trapping float-to-int conversions]: https://github.com/WebAssembly/nontrapping-float-to-int-conversions
[Sign-extension operators]: https://github.com/WebAssembly/sign-extension-ops
[Multi-value]: https://github.com/WebAssembly/multi-value
[JavaScript BigInt to WebAssembly i64 integration]: https://github.com/WebAssembly/JS-BigInt-integration
[Reference Types]: https://github.com/WebAssembly/reference-types
[Bulk memory operations]: https://github.com/WebAssembly/bulk-memory-operations
[Fixed-width SIMD]: https://github.com/webassembly/simd
[Custom Annotation Syntax in the Text Format]: https://github.com/WebAssembly/annotations
[Exception handling]: https://github.com/WebAssembly/exception-handling
[Typed Function References]: https://github.com/WebAssembly/function-references
[Garbage collection]: https://github.com/WebAssembly/gc
[Multiple memories]: https://github.com/WebAssembly/multi-memory
[Tail call]: https://github.com/WebAssembly/tail-call
[Threads]: https://github.com/webassembly/threads
[Type Reflection for WebAssembly JavaScript API]: https://github.com/WebAssembly/js-types
[Web Content Security Policy]: https://github.com/WebAssembly/content-security-policy
[Memory64]: https://github.com/WebAssembly/memory64
[Branch Hinting]: https://github.com/WebAssembly/branch-hinting
[Extended Constant Expressions]: https://github.com/WebAssembly/extended-const
[Relaxed SIMD]: https://github.com/WebAssembly/relaxed-simd
[JS Promise Integration]: https://github.com/WebAssembly/js-promise-integration

[🐌]: https://invidious.zapashcanon.fr/watch?v=XgK9Fd8ikxk

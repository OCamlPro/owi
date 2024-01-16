# [🐌] Owi [![build-badge]][build status] [![coverage-badge]][coverage percentage]

[Owi] is a toolchain to work with WebAssembly. It is written in [OCaml]. It provides a binary with many subcommands:

- [`owi c`]: a bug finding tool for C code that performs symbolic execution by compiling to Wasm and using our symbolic Wasm interpreter;
- [`owi fmt`]: a formatter for Wasm;
- [`owi opt`]: an optimizer for Wasm;
- [`owi run`]: a concrete Wasm interpreter;
- [`owi script`]: an interpreter for [Wasm specification tests];
- [`owi sym`]: a symbolic Wasm interpreter.

It also provides an OCaml library which allows for instance to [import OCaml functions in a Wasm module] in a type-safe way!

## Installation

`owi` can be installed with [opam]:

```shell-session
$ opam install owi
```

If you don't have `opam`, you can install it following the [how to install opam] guide.

If you can't or don't want to use `opam`, you can build the package with `dune build -p owi @install` but you'll first have to install the dependencies by yourself. You can find the list of dependencies in the [dune-project] file.

### Development version

To get the development version:

```shell-session
$ git clone git@github.com:OCamlPro/owi.git
$ cd owi
$ opam install . --deps-only
$ dune build -p owi @install
$ dune install
```

## Quickstart

### Concrete interpretation

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
$ dune exec owi -- run --debug test/passing/quickstart.wast
parsing      ...
checking     ...
grouping     ...
assigning    ...
rewriting    ...
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

If you want to run the file as a [reference test suite script], you can use the `script` command instead of the `run` one. This will allow you to add constructs like assertions and will also link the [spectest module], which provides function for e.g. printing.

If you're interested in the library part of Owi, here's how to get started:

```ocaml
# open Owi;;
# let filename = "test/passing/quickstart.wast";;
val filename : string = "test/passing/quickstart.wast"
# let m =
    match Parse.Module.from_file ~filename with
    | Ok script -> script
    | Error e -> failwith e;;
val m : Text.modul =
...
# let module_to_run, link_state =
    match Compile.until_link Link.empty_state ~unsafe:false ~optimize:false ~name:None m with
    | Ok v -> v
    | Error e -> failwith e;;
val module_to_run : '_weak1 Link.module_to_run =
...
val link_state : '_weak1 Link.state =
...
# let () =
    Log.debug_on := true;
    match Interpret.Concrete.modul link_state.envs module_to_run with
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

### Symbolic interpretation

The interpreter can also be used in symbolic mode. This allows to find which input values are leading to a trap.

Given a file `test/sym/mini_test.wast` with the following content:

<!-- $MDX file=test/sym/mini_test.wast -->
```wast
(module
  (import "symbolic" "i32" (func $gen_i32 (param i32) (result i32)))

  (func $start (local $x i32)
    (local.set $x (call $gen_i32 (i32.const 42)))
    (if (i32.lt_s (i32.const 5) (local.get $x)) (then
      unreachable
    )))

  (start $start)
)
```

You can run the symbolic interpreter through the `sym` command:
```sh
$ dune exec owi -- sym test/sym/mini_test.wast
Trap: unreachable
Model:
  (model
    (x_0 i32 (i32 6)))
Reached problem!
```

## Supported proposals

The 🐌 status means the proposal is not applicable to Owi.

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

### Talks

- [september 2023]: [ICFP OCaml track]
- [october 2023]: Wasm Research Day organized by the [WebAssembly Research Center] and hosted in Google Munich

### Spelling and pronunciation

Although the name Owi comes from an acronym (OCaml WebAssembly Interpreter), it must be written as a proper noun and only the first letter must be capitalized. It is possible to write the name in full lowercase when referring to the opam package or to the name of the binary.

The reason we chose this spelling rather than the fully capitalized version is that in French, Owi is pronounced [o’wi(ʃ)] which sounds like "Oh oui !" which means "Oh yes!". Thus it should be pronounced this way and not by spelling the three letters it is made of.

### Changelog

See [CHANGELOG].

### License

    Owi
    Copyright (C) 2021 Léo Andrès
    Copyright (C) 2021 Pierre Chambart

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

See [LICENSE].

### Fundings

This project was funded through the [NGI0 Core] Fund, a fund established by [NLnet] with financial support from the European Commission's [Next Generation Internet] programme. See [Owi project on NLnet].

[CHANGELOG]: ./CHANGES.md
[dune-project]: ./dune-project
[example]: ./example
[LICENSE]: ./LICENSE.md
[test suite]: ./test

[build-badge]: https://github.com/OCamlPro/owi/actions/workflows/build.yml/badge.svg
[build status]: https://github.com/ocamlpro/owi/actions
[coverage-badge]: https://raw.githubusercontent.com/ocamlpro/owi/gh-pages/coverage/badge.svg
[coverage percentage]: https://ocamlpro.github.io/owi/coverage
[documentation]: https://ocamlpro.github.io/owi/api/owi
[how to install opam]: https://opam.ocaml.org/doc/Install.html
[ICFP OCaml track]: https://icfp23.sigplan.org/home/ocaml-2023
[Next Generation Internet]: https://ngi.eu
[NLnet]: https://nlnet.nl
[NGI0 Core]: https://nlnet.nl/core
[OCaml]: https://ocaml.org
[october 2023]: https://invidious.zapashcanon.fr/watch?v=os_pknmiqmU
[opam]: https://opam.ocaml.org
[Owi]: https://ocamlpro.github.io/owi
[Owi project on NLnet]: https://nlnet.nl/project/OWI
[reference test suite script]: https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#scripts
[september 2023]: https://invidious.zapashcanon.fr/watch?v=IM76cMP3Eqo
[spectest module]: https://github.com/WebAssembly/spec/tree/main/interpreter#scripts
[video of our talk]: https://invidious.zapashcanon.fr/watch?v=os_pknmiqmU
[Wasm specification tests]: https://github.com/webassembly/spec/blob/main/interpreter/readme.md#spectest-host-module
[WebAssembly]: https://webassembly.org
[WebAssembly Research Center]: https://www.cs.cmu.edu/wrc

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

[`owi c`]: ./example/c
[`owi fmt`]: ./example/fmt
[`owi opt`]: ./example/opt
[`owi run`]: ./example/run
[`owi script`]: ./example/script
[`owi sym`]: ./example/sym
[import OCaml functions in a Wasm module]: ./example/define_host_function

[🐌]: https://invidious.zapashcanon.fr/watch?v=XgK9Fd8ikxk

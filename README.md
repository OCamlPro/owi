# [🐌] Owi [![build-badge]][build status] [![coverage-badge]][coverage percentage]

[Owi] is a toolchain to work with WebAssembly. It is written in [OCaml]. It provides a binary with many subcommands:

- [`owi c`]: a bug finding tool for C code that performs symbolic execution by compiling to Wasm and using our symbolic Wasm interpreter;
- [`owi c++`]: a bug finding tool for C++ code that performs symbolic execution by compiling to Wasm and using our symbolic Wasm interpreter;
- [`owi conc`]: a concolic Wasm interpreter;
- [`owi fmt`]: a formatter for Wasm;
- [`owi opt`]: an optimizer for Wasm;
- [`owi replay`]: run a module containing symbols with concrete values from a model produced by a previous symbolic execution
- [`owi run`]: a concrete Wasm interpreter;
- [`owi script`]: an interpreter for [Wasm scripts];
- [`owi sym`]: a symbolic Wasm interpreter;
- [`owi validate`]: a validator for Wasm modules;
- [`owi wasm2wat`]: a Wasm binary to text format translater;
- [`owi wat2wasm`]: a Wasm text to binary format translater.

It also provides an [OCaml library] which allows for instance to [import OCaml functions in a Wasm module] in a type-safe way!

We also have [a fuzzer] that is able to generate random *valid* Wasm programs. For now it has not been made available as a subcommand so you'll have to hack the code a little bit to play with it.

⚠️ For now, the optimizer and the formatter are quite experimental. The optimizer is well tested but only performs basic optimizations in an inefficient way. The formatter is mainly used for debugging purpose and is probably incorrect on some cases.

🧑‍🎓 We are looking for interns, have a look at the [internship labeled issues].

## Installation

`owi` can be installed with [opam]:

```shell-session
$ opam install owi
# if you intend to use symbolic execution you must install one solver
# you can choose any solver supported by smtml
# z3, colibri2, bitwuzla-cxx or cvc5 for instance
$ opam install z3
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

### Development setup

To get a proper development setup:

```shell-session
$ git clone git@github.com:OCamlPro/owi.git
$ cd owi
$ opam install . --deps-only --with-test --with-doc --with-dev-setup
$ dune build @all
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
| [Extended Constant Expressions]                  |  ✔️       |
| [Garbage collection]                             |  Ongoing |
| [Custom Annotation Syntax in the Text Format]    |  Ongoing |
| [Multiple memories]                              |  ❌      |
| [Memory64]                                       |  ❌      |
| [Exception handling]                             |  ❌      |
| [Branch Hinting]                                 |  ❌      |
| [Relaxed SIMD]                                   |  ❌      |
| [Threads]                                        |  ❌      |
| [Web Content Security Policy]                    |  🐌      |
| [JS Promise Integration]                         |  🐌      |
| [Type Reflection for WebAssembly JavaScript API] |  🐌      |

## About

### Publications and Talks

#### Publications

- [Owi: Performant Parallel Symbolic Execution Made Easy, an Application to WebAssembly], 2024
- [Exécution symbolique pour tous ou Compilation d'OCaml vers WebAssembly], 2024
- [Cross-Language Symbolic Runtime Annotation Checking], 2025

#### Talks

- [september 2023]: [ICFP OCaml track]
- [october 2023]: Wasm Research Day organized by the [WebAssembly Research Center] and hosted in Google Munich
- april 2024: [OUPS (OCaml UserS in Paris)]
- [november 2024]: [LVP working group] day of the [GdR GPL]
- [december 2024]: Léo Andrès' PhD defense
- january 2025: [JFLA 2025]

### Spelling and pronunciation

Although the name Owi comes from an acronym (OCaml WebAssembly Interpreter), it must be written as a proper noun and only the first letter must be capitalized. It is possible to write the name in full lowercase when referring to the opam package or to the name of the binary.

The reason we chose this spelling rather than the fully capitalized version is that in French, Owi is pronounced [o’wi(ʃ)] which sounds like "Oh oui !" which means "Oh yes!". Thus it should be pronounced this way and not by spelling the three letters it is made of.

### Changelog

See [CHANGELOG].

### License

    Owi
    Copyright (C) 2021-2024 OCamlPro

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

A few files have been taken from the WebAssembly reference interpreter. They are licensed under the Apache License 2.0 and have a different copyright which is stated in the header of the files.

Some code has been taken from the `base` library from Jane Street. It is licensed under the MIT License and have a different copyright which is stated in the header of the files.

Some code has been taken from the E-ACSL plugin of Frama-C. It is licensed under the GNU Lesser General Public License 2.1 and have a different copyright which is stated in the header of the files.

### Fundings

This project was partly funded through the [NGI0 Core] Fund, a fund established by [NLnet] with financial support from the European Commission's [Next Generation Internet] programme. See [Owi project on NLnet].

[CHANGELOG]: ./CHANGES.md
[dune-project]: ./dune-project
[example]: ./example
[LICENSE]: ./LICENSE.md
[test suite]: ./test

[build-badge]: https://github.com/OCamlPro/owi/actions/workflows/build.yml/badge.svg
[build status]: https://github.com/ocamlpro/owi/actions
[Cross-Language Symbolic Runtime Annotation Checking]: https://inria.hal.science/hal-04798756/file/cross_language_symbolic_runtime_annotation_checking.pdf
[coverage-badge]: https://raw.githubusercontent.com/ocamlpro/owi/gh-pages/coverage/badge.svg
[coverage percentage]: https://ocamlpro.github.io/owi/coverage
[december 2024]: https://www.zapashcanon.fr/phd#:~:text=after%20the%20defense.-,Defense,My%20slides%20are%20available%20here.,-%F0%9F%94%9D%20%7C%20nothing%20to%20hide
[documentation]: https://ocamlpro.github.io/owi/api/owi
[Exécution symbolique pour tous ou Compilation d'OCaml vers WebAssembly]: https://fs.zapashcanon.fr/pdf/manuscrit_these_leo_andres.pdf
[JFLA 2025]: https://jfla.inria.fr/jfla2025.html
[GdR GPL]: https://gdr-gpl.cnrs.fr/
[how to install opam]: https://opam.ocaml.org/doc/Install.html
[ICFP OCaml track]: https://icfp23.sigplan.org/home/ocaml-2023
[internship labeled issues]: https://github.com/OCamlPro/owi/labels/internship
[LVP working group]: https://gdrgpl.myxwiki.org/xwiki/bin/view/Main/GTs/GT%20Langages%20et%20v%C3%A9rification%20de%20programmes%20(LVP)
[Next Generation Internet]: https://ngi.eu
[NLnet]: https://nlnet.nl
[NGI0 Core]: https://nlnet.nl/core
[november 2024]: https://groupes.renater.fr/wiki/lvp/public/journee_lvp_novembre2024
[OCaml]: https://ocaml.org
[october 2023]: https://invidious.zapashcanon.fr/watch?v=os_pknmiqmU
[opam]: https://opam.ocaml.org
[OUPS (OCaml UserS in Paris)]: https://oups.frama.io
[Owi]: https://ocamlpro.github.io/owi
[Owi: Performant Parallel Symbolic Execution Made Easy, an Application to WebAssembly]: https://hal.science/hal-04627413
[Owi project on NLnet]: https://nlnet.nl/project/OWI
[reference test suite script]: https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#scripts
[september 2023]: https://invidious.zapashcanon.fr/watch?v=IM76cMP3Eqo
[Wasm scripts]: https://github.com/WebAssembly/spec/tree/main/interpreter#scripts
[video of our talk]: https://invidious.zapashcanon.fr/watch?v=os_pknmiqmU
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

[`owi c`]: example/c
[`owi c++`]: example/cpp
[`owi conc`]: example/conc
[`owi fmt`]: example/fmt
[`owi opt`]: example/opt
[`owi replay`]: example/replay
[`owi run`]: example/run
[`owi script`]: example/script
[`owi sym`]: example/sym
[`owi validate`]: example/validate
[`owi wasm2wat`]: example/wasm2wat
[`owi wat2wasm`]: example/wat2wasm
[import OCaml functions in a Wasm module]: example/define_host_function
[OCaml library]: example/lib
[a fuzzer]: test/fuzz

[🐌]: https://invidious.zapashcanon.fr/watch?v=XgK9Fd8ikxk

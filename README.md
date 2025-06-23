# [🐌] Owi [![build-badge]][build status] [![coverage-badge]][coverage percentage]

**Owi** is an open-source framework for advanced WebAssembly analysis and manipulation, with a focus on practical symbolic execution and robust tooling.
It is designed for researchers, engineers, programming language enthusiasts and practitioners requiring precise, flexible, and extensible support program reasoning.

Owi provides three primary components:

###  🔬 Symbolic Execution Engine for Wasm, C, C++, Rust, and Zig

Owi includes a cross-language symbolic execution engine supporting:
- **Automated testing, bug finding and pentesting** through symbolic testing and constraint solving;
- **Solver-aided programming** for problem solving and synthesis tasks;
- **Test case generation** aligned with advanced coverage criteria;
- **Formal verification**: prove properties or find counterexamples in real-world programs.

The engine is designed for precision, scalability, interoperability across languages, and extensibility toward both experimental and applied verification use-cases.
It offers a practical path from fuzzing to formal proofs.

### 🔧 The Wasm Swiss Army Knife

Owi offers a set of practical tools for Wasm development and analysis:
- **Formatter** for Wasm modules;
- **Interpreter** for `.wasm`, `.wat`, and `.wast` files;
- **Specification-compliant validator**;
- **Bidirectional translation** between binary (`.wasm`) and text (`.wat`) formats;
- **Randomized fuzzer** generating valid (well-typed) Wasm modules.

These tools aim to support everyday development tasks as well as research on program analysis, fuzzing, and program transformation.

### 🐪  Native OCaml Library for Wasm Integration

Owi also provides a library for:
- **Embedding Wasm modules** into OCaml applications;
- **Importing OCaml functions** into Wasm modules with full type safety.

This allows for tightly integrating Wasm-based computation within OCaml-based systems while maintaining strong type guarantees.

### Documentation

A [detailed documentation is available], including tutorials, how-to guides, explanation and design notes, many references and instructions for contributing.

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

A few files have been taken from the Wasm reference interpreter. They are licensed under the Apache License 2.0 and have a different copyright which is stated in the header of the files.

Some code has been taken from the `base` library from Jane Street. It is licensed under the MIT License and have a different copyright which is stated in the header of the files.

Some code has been taken from the E-ACSL plugin of Frama-C. It is licensed under the GNU Lesser General Public License 2.1 and have a different copyright which is stated in the header of the files.


[LICENSE]: ./LICENSE.md

[build-badge]: https://github.com/OCamlPro/owi/actions/workflows/build-ubuntu.yml/badge.svg
[build status]: https://github.com/ocamlpro/owi/actions
[coverage-badge]: https://raw.githubusercontent.com/ocamlpro/owi/gh-pages/coverage/badge.svg
[coverage percentage]: https://ocamlpro.github.io/owi/coverage
[detailed documentation is available]: https://ocamlpro.github.io/owi/

[🐌]: https://youtu.be/XgK9Fd8ikxk

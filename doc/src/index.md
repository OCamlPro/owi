# Welcome to Owi

**Owi** is an open-source framework for advanced [WebAssembly] analysis and manipulation, with a focus on practical symbolic execution and robust tooling.
It is designed for researchers, engineers, programming language enthusiasts and practitioners requiring precise, flexible, and extensible support program reasoning.

Owi provides three primary components:

###  🔬 [Symbolic Execution Engine](symex/overview.md) for [Wasm](symex/quickstart/wasm.md), [C](symex/quickstart/c.md), [C++](symex/quickstart/cpp.md), [Rust](symex/quickstart/rust.md), and [Zig](symex/quickstart/zig.md)

Owi includes a cross-language symbolic execution engine supporting:
- [**Automated testing, bug finding and pentesting**](symex/bugfinding.md) through symbolic testing and constraint solving;
- [**Solver-aided programming**](symex/sap.md) for problem solving and synthesis tasks;
- [**Test case generation**](symex/testcase_generation.md) aligned with advanced coverage criteria;
- [**Formal verification**](symex/verification.md): prove properties or find counterexamples in real-world programs.

The engine is designed for precision, scalability, interoperability across languages, and extensibility toward both experimental and applied verification use-cases.
It offers a practical path from fuzzing to formal proofs.

### 🔧 The [Wasm Swiss Army Knife](./wasm-toolkit/overview.md)

Owi offers a set of practical tools for Wasm development and analysis:
- **Formatter** and **optimizer** for Wasm modules;
- **Interpreter** for `.wasm`, `.wat`, and even [Wasm scripts]  (`.wast`) files that are used in the [reference test suite];
- **Specification-compliant validator**;
- **Bidirectional translation** between binary (`.wasm`) and text (`.wat`) formats;
- **Randomized fuzzer** generating valid (well-typed) Wasm modules.

These tools aim to support everyday development tasks as well as research on program analysis, fuzzing, and program transformation.

### 🐪  Native [OCaml Library for Wasm](ocaml-api/overview.md) Integration

Owi is written in [OCaml], thus it also provides a library for:
- **Embedding Wasm modules** into OCaml applications;
- **Importing OCaml functions** into Wasm modules with full type safety.

This allows for tightly integrating Wasm-based computation within OCaml-based systems while maintaining strong type guarantees.

### ⚠️ Warning

For now, the optimizer and the formatter are quite experimental. The optimizer is well tested but only performs basic optimizations in an inefficient way. The formatter is mainly used for debugging purpose and is probably incorrect on some cases. Moreover, the *concolic* mode is currently broken, use the symbolic one.

### 🧑‍🎓Want to join us?

We are looking for interns, have a look at the [internship labeled issues].

[internship labeled issues]: https://github.com/OCamlPro/owi/labels/internship
[OCaml]: https://ocaml.org
[reference test suite]: https://github.com/WebAssembly/spec/blob/main/interpreter/README.md#scripts
[Wasm scripts]: https://github.com/WebAssembly/spec/tree/main/interpreter#scripts
[WebAssembly]: https://webassembly.org

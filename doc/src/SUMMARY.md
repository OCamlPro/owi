# Summary

[Home](index.md)
[Installation](installation.md)
[Supported Wasm proposals](proposals.md)

# Symbolic Execution Engine

- [Overview](symex/overview.md)
    - [Symbolic Execution 101](symex/symbolic-execution-101.md)
    - [Quickstart](symex/quickstart.md)
- [Bug-Finding, Testing & Pen-testing](symex/bugfinding.md)
    - [Examples of Bug Finding](symex/bugfinding/examples.md)
    - [How to Speed it Up](symex/bugfinding/optimizations.md)
    - [Replaying a model](symex/bugfinding/replay.md)
    - [Checking iso-behaviour of two modules](symex/bugfinding/iso.md)
    - [Comparison to Fuzzing and Abstract Interpretation](symex/bugfinding/comparison.md)
    - [Bugs Found by Owi](symex/bugfinding/bugs-found.md)

- [Solver-Aided Programming](symex/sap.md)
    - [Examples of Problem Solving](symex/sap/examples.md)
    - [How to Speed it Up](symex/sap/optimizations.md)
    - [Comparison to Rosette, Prolog, etc.](symex/sap/comparison.md)

- [Test-Case Generation](symex/testcase-generation.md)
    - [Labels](symex/testcase-generation/labels.md)

- [Verification and Proof of Programs](symex/verification.md)
    - [Comparison with Deductive Verification and Abstract Interpretation](symex/verification/comparison.md)
    - [E-ACSL](symex/verification/eacsl.md)
    - [Weasel](symex/verification/weasel.md)

- [Going Further](symex/further.md)
    - [Comparison with Other Engines](symex/further/comparison.md)
    - [Talks and Papers](symex/further/talks-papers.md)
    - [Commands and Options](symex/further/commands.md)
    - [API: Symbols and Helpers](symex/further/api.md)

# WebAssembly Toolkit
- [Overview](wasm-toolkit/overview.md)
- [Comparison with Other Tools](wasm-toolkit/comparison.md)

# Man pages
- [`owi`](manpages/owi.md)
    - [`owi analyze`](manpages/owi-analyze.md)
    - [`owi c`](manpages/owi-c.md)
    - [`owi c++`](manpages/owi-cpp.md)
    - [`owi conc`](manpages/owi-conc.md)
    - [`owi fmt`](manpages/owi-fmt.md)
    - [`owi instrument`](manpages/owi-instrument.md)
    - [`owi iso`](manpages/owi-iso.md)
    - [`owi replay`](manpages/owi-replay.md)
    - [`owi run`](manpages/owi-run.md)
    - [`owi rust`](manpages/owi-rust.md)
    - [`owi script`](manpages/owi-script.md)
    - [`owi sym`](manpages/owi-sym.md)
    - [`owi validate`](manpages/owi-validate.md)
    - [`owi version`](manpages/owi-version.md)
    - [`owi wasm2wat`](manpages/owi-wasm2wat.md)
    - [`owi wat2wasm`](manpages/owi-wat2wasm.md)
    - [`owi zig`](manpages/owi-zig.md)

# Public OCaml API
- [Overview](ocaml-api/overview.md)
- [How to Define Custom Functions](ocaml-api/custom-functions.md)
- [Generated API Documentation](ocaml-api/odoc.md)

# Hacking
- [Development Setup](hacking/setup.md)
- [Coding Guidelines](hacking/guidelines.md)
- [Documentation](hacking/doc.md)
- [Testing](hacking/testing.md)
- [Benchmarking](hacking/benchmarking.md)

# About
- [History of Owi](about/history.md)
- [Authors and Contributors](about/authors.md)
- [License](about/license.md)
- [Funding](about/fundings.md)
- [Changelog](about/changelog.md)
- [Projects and People Using Owi](about/users.md)

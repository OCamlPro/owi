## unreleased

- add `owi replay` command to check a model produced by symbolic execution on a concrete execution
- add `owi instrument` to instrument Webassembly module annotated by Weasel specification language
- add `--srac` option to `sym` and `conc` cmd
- add `--rac` option to `run`, `sym` and `conc` cmd
- add `--output` option to `wat2wasm`, `wasm2wat` and `opt` cmd
- Change `free` prototype to now return a pointer on which tracking is deactivated
- add `--emit-file` option to `wasm2wat` to emit .wat files
- adds flags `--fail-on-trap-only` and `fail-on-assertion-only`
- parameterize the `Thread` module on the symbolic memory and the `Choice_monad` module on a Thread
- add `owi_char` and `owi_bool` function to create char and bool symbolic values
- adds a `Mem` argument to external function to allow direct manipulation of the memory.
- add `owi c --e-acsl` to instrument the program by Frama-C's E-ACSL plugin before subsequent steps
- support other solvers through the `--solver` option (Z3, Colibri2, Bitwuzla and CVC5)
- support the extended constant expressions proposal
- `owi opt` and `owi sym` can run on `.wasm` files directly
- remove dependency on `wabt`
- better API for `Parse`, `Compile` and `Simplified` (renamed to `Binary`), added a `Binary_to_text` module
- add `owi conc` subcommands and `owi c --concolic`: concolic mode
- start benchmarking against test-comp
- fix handling of `select` and `call_indirect` in the text format
- add `owi wat2wasm` subcommand
- add a `no-assert-failure-expression-printing` flag to `owi c`, `owi sym` and `owi conc`
- add `owi version` subcommand
- add `owi c++` subcommand
- add `owi rust` subcommand
- add event logger
- add `owi_range` primitive
- add `owi zig` subcommand
- add `--model-format` option
- add `--entry-point` option
- add `--invoke-with-symbols` option
- add `owi iso` subcommand
- add `--workspace` option
- add `--model-out-file` option
- add `owi_label` primitive
- add `--with-breadcrumbs` option
- add path-condition slicing
- add `owi_invisible_bool` primitive
- add `owi_label_is_covered` primitive
- add scopes with `owi_open_scope` and `owi_close_scope` primitives
- start support for simd instructions
- add `--exploration` option
- add `--bench` option
- add `--no-ite-for-select` option
- support the multi-memory proposal

## 0.2 - 2024-04-24

- use a subcommand system for the `owi` binary
- add `owi c`, `owi fmt`, `owi opt`, `owi sym`, `owi validate` and `owi wasm2wat` subcommands
- add a fuzzer
- add a profiling mode
- make `spectec` print stuff for real

## 0.1 - 2023-01-14

- first release

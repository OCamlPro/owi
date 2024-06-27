## unreleased

- support other solvers through the `--solver` option (Z3, Colibri2, Bitwuzla and CVC5)
- support the extended constant expressions proposal
- `owi opt` and `owi sym` can run on `.wasm` files directly
- remove dependency on `wabt`
- better API for `Parse`, `Compile` and `Simplified` (renamed to `Binary`), added a `Binary_to_text` module
- add `owi conc` subcommands and `owi c --concolic`: concolic mode
- start benchmarking against test-comp
- fix handling of `select` and `call_indirect` in the text format
- add `owi wat2wasm` subcommand
- moving memory existence and alignment from rewrite module to typecheck module

## 0.2 - 2024-04-24

- use a subcommand system for the `owi` binary
- add `owi c`, `owi fmt`, `owi opt`, `owi sym`, `owi validate` and `owi wasm2wat` subcommands
- add a fuzzer
- add a profiling mode
- make `spectec` print stuff for real

## 0.1 - 2023-01-14

- first release

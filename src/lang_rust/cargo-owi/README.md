# cargo-owi

A Cargo subcommand to run Rust programs through [owi](https://ocamlpro.github.io/owi/)'s symbolic execution engine for automated bug finding.

`cargo-owi` compiles your crate to WebAssembly and passes it to `owi` for symbolic analysis. 

## Prerequisites

- [owi](https://ocamlpro.github.io/owi/) must be installed and available on `$PATH`
- The `wasm32-unknown-unknown` target must be installed: `rustup target add wasm32-unknown-unknown`
- Your crate should use the [`owi`](https://crates.io/crates/owi) library to declare symbolic values and assumptions

## Installation

```sh
cargo install cargo-owi
```

## Usage

```
cargo owi sym [OPTIONS] [-- [OWI_OPTIONS]]
```

Run symbolic execution on the current package:

```sh
cargo owi sym
```

Pass extra options to owi after `--`:

```sh
cargo owi sym -- --timeout 60
```

### Options

| Flag | Description |
|------|-------------|
| `--manifest-path <PATH>` | Path to `Cargo.toml` |
| `--features <FEATURES>` | Space- or comma-separated list of features to activate |
| `-p`, `--package <SPEC>` | Package to analyze (see `cargo help pkgid`) |

Any arguments after `--` are forwarded verbatim to `owi sym`.


## Example

Add symbolic inputs to your code using the `owi` crate:

```rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn main() {
    let x: i32 = owi::symbolic();
    let y: i32 = owi::symbolic();
    owi::assume(x > 0 && y > 0);
    assert!(add(x, y) > 0);
}
```

Then run:

```sh
cargo owi sym
```

owi will explore execution paths symbolically and report any path that violates an assertion or triggers undefined behavior.



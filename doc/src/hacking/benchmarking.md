# Benchmarking

## Landmarks

```shell-session
OCAML_LANDMARKS=on dune exec --instrument-with landmarks --profile release -- owi run test/run/binary_loop.wasm
```

Note: it seems landmarks is not compatible with domains and thus won't work with most sub-commands.

## Test-comp

See [Symbocalypse].

[Symbocalypse]: https://github.com/OCamlPro/symbocalypse

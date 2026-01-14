# Benchmarking

## Landmarks

```shell-session
OCAML_LANDMARKS=on dune exec --instrument-with landmarks --profile release -- owi run test/run/binary_loop.wasm
```

See the discussion in [#871] to understand landmarks limitations in Owi.

## Test-comp

See [Symbocalypse].

[#871]: https://github.com/OCamlPro/owi/pull/871
[Symbocalypse]: https://github.com/OCamlPro/symbocalypse

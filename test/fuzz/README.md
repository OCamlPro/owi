# Fuzzer

This fuzzer generates random (valid) Wasm programs.

Once a random Wasm program was generated, it performs differential testing with different to test various stuff:

- `optimize_fuzzing` will compare the original program against its optimized (with `owi opt`) version by running them both in the concrete interpreter and comparing the output;
- `reference_fuzzing` will compare the output of the program in the concrete interpreter and in Wasm reference interpreter;
- `symbolic_fuzzing` will compare the output of the program in the concrete interpreter and in the symbolic interpreter.

You can choose which of the different kind of differential testing should be performed in `param.ml`.
This file also contains other parameters, feel free to tune them.

## Fuzzing mode

There are two ways to run the fuzzer.

In quick-check mode:

```shell-session
$ dune exec ./fuzzer.exe
```

In AFL mode:

```shell-session
$ afl-fuzz -i in/ -o out -- ../../_build/default/test/fuzz/fuzzer.exe @@
```


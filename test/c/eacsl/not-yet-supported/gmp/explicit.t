  $ owi c --e-acsl explicit.c --no-value 2>&1 | grep -v "wasm-ld" | grep -v "gmp.h" > err.txt || false

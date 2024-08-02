  $ owi c --e-acsl implicit.c --no-value 2>&1 | grep -v "wasm-ld" | grep -v "gmp.h" | grep -v "clang"
  [e-acsl] implicit.c:10: Warning: 
    E-ACSL construct `assigns' is not yet supported. Ignoring annotation.
  [e-acsl] implicit.c:10: Warning: 
    E-ACSL construct `assigns clause in behavior' is not yet supported.
    Ignoring annotation.
  Clang failed: run with --debug to get the full error message

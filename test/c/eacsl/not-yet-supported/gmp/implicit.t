  $ owi c --e-acsl implicit.c --no-value 2>&1 | grep -v "wasm-ld" | grep -v "gmp.h"
  [e-acsl] implicit.c:10: Warning: 
    E-ACSL construct `assigns' is not yet supported. Ignoring annotation.
  [e-acsl] implicit.c:10: Warning: 
    E-ACSL construct `assigns clause in behavior' is not yet supported.
    Ignoring annotation.
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  clang failed: run with --debug to get the full error message

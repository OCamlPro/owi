  $ owi c --e-acsl explicit.c --no-value 2>&1 | grep -v "wasm-ld"
  [kernel:typing:implicit-function-declaration] /usr/include/x86_64-linux-gnu/gmp.h:1812: Warning: 
    __builtin_expect is a compiler builtin, only allowed for GCC-based machdeps; see option -machdep or run 'frama-c -machdep help' for the list of available machdeps
  [kernel:typing:implicit-function-declaration] explicit.c:11: Warning: 
    __builtin_constant_p is a compiler builtin, only allowed for GCC-based machdeps; see option -machdep or run 'frama-c -machdep help' for the list of available machdeps
  [e-acsl] /usr/include/x86_64-linux-gnu/gmp.h:1761: Warning: 
    E-ACSL construct `assigns clause in behavior' is not yet supported.
    Ignoring annotation.
  [e-acsl] /usr/include/x86_64-linux-gnu/gmp.h:1792: Warning: 
    E-ACSL construct `assigns clause in behavior' is not yet supported.
    Ignoring annotation.
  [e-acsl] /usr/include/x86_64-linux-gnu/gmp.h:1825: Warning: 
    E-ACSL construct `assigns clause in behavior' is not yet supported.
    Ignoring annotation.
  [e-acsl] /usr/include/x86_64-linux-gnu/gmp.h:1872: Warning: 
    E-ACSL construct `assigns clause in behavior' is not yet supported.
    Ignoring annotation.
  clang: error: linker command failed with exit code 1 (use -v to see invocation)
  Clang failed: run with --debug to get the full error message

  $ owi c --e-acsl ./strong_type.c --no-value
  [kernel:annot-error] strong_type.c:4: Warning: unexpected token 'type'
  [kernel] User Error: warning annot-error treated as fatal error.
  [kernel] Frama-C aborted: invalid user input.
  run ['/home/laplace_demon/.opam/5.2.0+flambda/bin/frama-c' '-e-acsl'
       '-no-frama-c-stdlib' '-kernel-warn-key' 'CERT:MSC:38=inactive'
       '-verbose' '0'
       '-cpp-extra-args="-I/home/laplace_demon/WorkPlace/owi/_build/install/default/share/owi/libc"'
       './strong_type.c' '-then-last' '-print' '-ocode'
       './strong_type_instrumented.c']: exited with 1
  [26]

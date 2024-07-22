  $ owi c --e-acsl ./general_measure.c
  [kernel:annot-error] general_measure.c:24: Warning: unexpected token ''
  [kernel] User Error: warning annot-error treated as fatal error.
  [kernel] Frama-C aborted: invalid user input.
  run ['/home/laplace_demon/.opam/5.2.0+flambda/bin/frama-c' '-e-acsl'
       '-no-frama-c-stdlib' '-kernel-warn-key' 'CERT:MSC:38=inactive'
       '-verbose' '0'
       '-cpp-extra-args="-I/home/laplace_demon/WorkPlace/owi/_build/install/default/share/owi/libc"'
       './general_measure.c' '-then-last' '-print' '-ocode'
       './general_measure_instrumented.c']: exited with 1
  [26]

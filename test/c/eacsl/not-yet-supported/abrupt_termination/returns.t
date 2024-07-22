  $ owi c --e-acsl ./returns.c
  [e-acsl] returns.c:8: User Error: 
    Term '\at(x,__invalid_label)' was used before being translated.
    This usually happens when using a label defined after the place
    where the Term should be translated
  [kernel] Plug-in e-acsl aborted: invalid user input.
  run ['/home/laplace_demon/.opam/5.2.0+flambda/bin/frama-c' '-e-acsl'
       '-no-frama-c-stdlib' '-kernel-warn-key' 'CERT:MSC:38=inactive'
       '-verbose' '0'
       '-cpp-extra-args="-I/home/laplace_demon/WorkPlace/owi/_build/install/default/share/owi/libc"'
       './returns.c' '-then-last' '-print' '-ocode' './returns_instrumented.c']: exited with 1
  [26]

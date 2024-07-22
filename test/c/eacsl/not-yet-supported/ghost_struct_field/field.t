  $ owi c --e-acsl ./field.c
  [kernel] field.c:9: User Error: 
    redefinition of 'List' in the same scope.
    Previous declaration was at field.c:5
  [kernel] field.c:17: User Error: 
    Cannot find field order in type struct List
    15            loop variant length;
    16        */
    
    17        while (node != NULL) {
    18            node = node->next;
    19            //@ ghost length --;
    20        }
    
    21    }
    22
  [kernel] Frama-C aborted: invalid user input.
  run ['/home/laplace_demon/.opam/5.2.0+flambda/bin/frama-c' '-e-acsl'
       '-no-frama-c-stdlib' '-kernel-warn-key' 'CERT:MSC:38=inactive'
       '-verbose' '0'
       '-cpp-extra-args="-I/home/laplace_demon/WorkPlace/owi/_build/install/default/share/owi/libc"'
       './field.c' '-then-last' '-print' '-ocode' './field_instrumented.c']: exited with 1
  [26]

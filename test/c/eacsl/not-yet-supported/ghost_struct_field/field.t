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
  owi: [ERROR] Frama-C failed: run with --debug to get the full error message
  [26]

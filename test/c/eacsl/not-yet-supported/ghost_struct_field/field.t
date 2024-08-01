  $ owi c --e-acsl ./field.c
  [kernel] field.c:9: User Error: 
    redefinition of 'List' in the same scope.
    Previous declaration was at field.c:5
  [kernel] field.c:13: User Error: 
    Cannot find field order in type struct List
    11    }; */
    12    
    13    void traverse(struct List* node) /*@ ghost (int length) */ {
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    14        /*@ loop invariant length == node->order;
    15            loop variant length;
  [kernel] Frama-C aborted: invalid user input.
  Frama-C failed: run with --debug to get the full error message
  [26]

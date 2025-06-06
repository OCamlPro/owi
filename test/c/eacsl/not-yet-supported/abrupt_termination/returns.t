  $ owi c --e-acsl ./returns.c
  [e-acsl] returns.c:8: User Error: 
    Term '\at(x,__invalid_label)' was used before being translated.
    This usually happens when using a label defined after the place
    where the Term should be translated
  [kernel] Plug-in e-acsl aborted: invalid user input.
  owi: [ERROR] Frama-C failed: run with -vv to get the full error message if it was not displayed above
  [26]

  $ dune exec owi -- c --testcomp ./simple.c
  Assert failure: (i32.ne (i32.mul symbol_0 symbol_0) (i32 0))
  Model:
    (model
      (symbol_0 (i32 0)))
  found a bug while performing symbolic execution!
  [13]

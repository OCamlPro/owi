memory stuff:
  $ dune exec owi -- sym memory.wat --no-stop-at-failure
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Trap: unreachable
  Model:
    (model
  ...
  Reached 11 problems!
  $ dune exec owi -- sym grow.wat --no-stop-at-failure
  Trap: out of bounds memory access
  Model:
    (model
      (symbol_0 (i32 1)))
  Reached 1 problems!

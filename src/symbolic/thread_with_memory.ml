include Thread.Make (Symbolic_memory)

let project (th : t) : Thread_without_memory.t * _ =
  let projected =
    { Thread_without_memory.symbols = th.symbols
    ; symbol_set = th.symbol_set
    ; pc = th.pc
    ; memories = ()
    ; tables = th.tables
    ; globals = th.globals
    ; breadcrumbs = th.breadcrumbs
    }
  in
  let backup = th.memories in
  (projected, backup)

let restore backup th =
  { symbols = th.Thread_without_memory.symbols
  ; symbol_set = th.symbol_set
  ; pc = th.pc
  ; memories = backup
  ; tables = th.tables
  ; globals = th.globals
  ; breadcrumbs = th.breadcrumbs
  }

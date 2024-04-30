return instructions:
  $ owi opt return.wat
  (module
    (table $tab 1 1 (ref null func))
    (func $return (result i32)
      i32.const 42
      return
    )
    (func $return_call (result i32)
      return_call 0
    )
    (func $return_call_indirect (result i32)
      i32.const 0
      return_call_indirect 0  (result i32)
    )
    (func $start
      call 0
      drop
      call 1
      drop
      call 2
      drop
    )
    (elem (table 0) (offset i32.const 0) (ref null func) (item ref.func 0))
    (start 3)
  )

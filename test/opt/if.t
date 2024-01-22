if then else instruction:
  $ dune exec -- owi opt if.wat
  (module
    (func $start
      (block (result i32)
        i32.const 42)
      drop
      (block (result i32)
        i32.const 24)
      drop
    )
    (start 0)
  )

if then else instruction:
  $ owi opt if.wat > if.opt.wat
  $ cat if.opt.wat
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
  $ owi run if.opt.wat

i32 / i64 binary operations:
  $ owi opt ibinop.wat > ibinop.opt.wat
  $ cat ibinop.opt.wat
  (module
    (type (func))
    (func $start
      
    )
    (start 0)
  )
  $ owi run ibinop.opt.wat

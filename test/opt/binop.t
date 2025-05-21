binary operations instructions:
  $ owi opt binop.wat > binop.opt.wat
  $ cat binop.opt.wat
  (module
    (type (func))
    (func $i32binop
      
    )
    (func $i64binop
      
    )
    (func $f32binop
      
    )
    (func $f64binop
      
    )
    (func $start
      call 0
      call 1
      call 2
      call 3
    )
    (start 4)
  )
  $ owi run binop.opt.wat

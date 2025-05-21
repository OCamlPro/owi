**.eq eqz lt gt ...  instructions:
  $ owi opt relop_testop.wat > relop_testop.opt.wat
  $ cat relop_testop.opt.wat
  (module
    (type (func))
    (func $i32relop
      
    )
    (func $i64relop
      
    )
    (func $f32relop
      
    )
    (func $f64relop
      
    )
    (func $itestop
      
    )
    (func $start
      call 0
      call 1
      call 2
      call 3
      call 4
    )
    (start 5)
  )
  $ owi run relop_testop.opt.wat

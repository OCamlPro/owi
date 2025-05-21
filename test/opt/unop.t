unop () instructions:
  $ owi opt unop.wat > unop.opt.wat
  $ cat unop.opt.wat
  (module
    (type (func))
    (func $iunop
      
    )
    (func $f32unop
      
    )
    (func $f64unop
      
    )
    (func $start
      call 0
      call 1
      call 2
    )
    (start 3)
  )
  $ owi run unop.opt.wat

**.trunc_** **.trunc_sat_** instructions:
  $ owi opt trunc.wat > trunc.opt.wat
  $ cat trunc.opt.wat
  (module
    
    (type (sub final  (func)))
    (func $trunc
      
    )
    (func $trunc_sat
      
    )
    (func $start
      call 0
      call 1
    )
    (start 2)
  )
  $ owi run trunc.opt.wat

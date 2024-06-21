select instruction:
  $ owi opt select.wat > select.opt.wat
  $ cat select.opt.wat
  (module
    
    (type (sub final  (func)))
    (func $start
      
    )
    (start 0)
  )
  $ owi run select.opt.wat

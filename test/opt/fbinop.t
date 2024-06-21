f32 / f64 binary operations:
  $ owi opt fbinop.wat > fbinop.opt.wat
  $ cat fbinop.opt.wat
  (module
    
    (type (sub final  (func)))
    (func $start
      
    )
    (start 0)
  )
  $ owi run fbinop.opt.wat

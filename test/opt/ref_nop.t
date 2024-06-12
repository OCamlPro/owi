ref.null ref.is_null nop instructions:
  $ owi opt ref_nop.wat > ref_nop.opt.wat
  $ cat ref_nop.opt.wat
  (module
    (func $start
      
    )
    (start 0)
  )
  $ owi run ref_nop.opt.wat

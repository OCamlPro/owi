f32.demote_f64 f64.promote_f32 instructions:
  $ owi opt demote_promote.wat > demote_promote.opt.wat
  $ cat demote_promote.opt.wat
  (module
    (type (sub final  (func)))
    (func $start
      
    )
    (start 0)
  )
  $ owi run demote_promote.opt.wat

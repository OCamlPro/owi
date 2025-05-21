i**.extend_** i32.wrap_i64 instructions:
  $ owi opt extend_wrap.wat > extend_wrap.opt.wat
  $ cat extend_wrap.opt.wat
  (module
    (type (func))
    (func $extend
      
    )
    (func $wrap
      
    )
    (func $start
      call 0
      call 1
    )
    (start 2)
  )
  $ owi run extend_wrap.opt.wat

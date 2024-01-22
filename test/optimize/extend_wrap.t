i**.extend_** i32.wrap_i64 instructions:
  $ dune exec -- owi opt extend_wrap.wat
  (module
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

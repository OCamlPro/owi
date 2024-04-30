drop instruction:
  $ owi opt drop.wat
  (module
    (global $g i32 i32.const 0)
    (func $start
      call 1
      call 2
    )
    (func $const
      
    )
    (func $local_global
      
    )
    (start 0)
  )

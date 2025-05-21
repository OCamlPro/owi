drop instruction:
  $ owi opt drop.wat > drop.opt.wat
  $ cat drop.opt.wat
  (module
    (type (func))
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
  $ owi run drop.opt.wat

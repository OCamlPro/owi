test debug printing:
  $ owi opt drop.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] optimizing   ...
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

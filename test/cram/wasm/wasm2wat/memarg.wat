(module
  (memory 1)
  (func $f
    i32.const 4
    i32.const 99999999
    i32.store

    i32.const 1
    i32.load align=2

    drop
  )
  (start $f))

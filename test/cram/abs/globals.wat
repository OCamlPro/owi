(module
  (global $x (mut i32) (i32.const 42))
  
  (func $start
    global.get $x
    i32.const 1
    i32.add
    drop
  )
  
  (start $start)
)

(module
  (@contract $plus_three
    (ensures (= result (+ $x 3)))
  )
  (func $plus_three (param $x i32) (result i32)
    local.get $x
    i32.const 3
    i32.add
  )
  (func $start
    i32.const 42
    call $plus_three
    drop
  )
  (start $start)
)

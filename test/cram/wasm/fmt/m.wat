(module
  (func $f (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.add
  )
  (func $start
    i32.const 22
    i32.const 20
    call $f
    drop
  )
  (start $start)
)

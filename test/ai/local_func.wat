(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    local.get $rhs
    i32.add
    return
  )
  (func $start
    i32.const 42
    i32.const 28
    call $add
    return
  )

  (start $start)
)

(module
  (func $mean1 (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.and

    local.get $x
    local.get $y
    i32.xor
    i32.const 1
    i32.shr_s

    i32.add
  )

  (func $mean2 (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.add
    i32.const 2
    i32.div_s
  )

  (func $check  (export "check") (param $x i32) (param $y i32)
    local.get $x
    local.get $y
    call $mean1

    local.get $x
    local.get $y
    call $mean2

    i32.ne
    if
      unreachable
    end
  )
)

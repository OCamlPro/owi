(module

  (func (export "unused2") (param $x i32) (param $y i64) (result i64)
    local.get $x
    (if (then (unreachable)))
    local.get $y
  )

  (func (export "mul") (param $x i32) (param $y i32) (result i32)
    local.get $y
    local.get $x
    i32.mul
    i32.const 1
    i32.add
  )
)

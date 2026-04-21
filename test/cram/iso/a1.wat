(module

  (func (export "unused1") (param $x i32)
    local.get $x
    drop
  )

  (func (export "mul") (param $x i32) (param $y i32) (result i32)
    local.get $x
    local.get $y
    i32.mul
  )
)

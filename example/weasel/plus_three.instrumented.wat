(module
  (import "symbolic" "assert" (func $assert  (param i32)))
  (type (sub final  (func (param $x i32) (result i32))))
  (type (sub final  (func)))
  (type (sub final  (func (param i32))))
  (type (sub final  (func (result i32))))
  (func $plus_three (param $x i32) (result i32)
    local.get 0
    i32.const 3
    i32.add
  )
  (func $start
    i32.const 42
    call 3
    drop
  )
  (func $__weasel_plus_three (param $x i32) (result i32) (local $__weasel_temp i32) (local $__weasel_res_0 i32)
    local.get 0
    call 1
    local.set 2
    local.get 2
    local.get 0
    i32.const 3
    i32.add
    i32.eq
    call 0
    local.get 2
  )
  (start 2)
)
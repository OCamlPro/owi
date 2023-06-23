(module
  (import "print" "i32" (func $print_i32 (param i32)))
  (import "symbolic" "i32" (func $gen_i32 (param i32) (result i32)))
  (func $factR (export "fact") (param $n i32) (result i32)
    local.get $n
    local.get $n
    i32.const 1
    i32.eq
    br_if 0
    i32.const 1
    i32.sub
    call $factR
    local.get $n
    i32.mul
  )

  (func $start
    (local $x i32)
    (i32.const 123)
    (call $print_i32)
    (local.set $x (call $gen_i32 (i32.const 1)))
    (if (i32.lt_s (i32.const 5) (local.get $x)) (then
      unreachable
    ))
    (if (i32.gt_s (i32.const 1) (local.get $x)) (then
      unreachable
    ))
    (local.get $x)
    (call $factR)
    (call $print_i32))

  (start $start)
)

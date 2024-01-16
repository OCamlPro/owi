(module
  (import "symbolic" "i32" (func $gen_i32 (param i32) (result i32)))

  (func $start (local $x i32)
    (local.set $x (call $gen_i32 (i32.const 42)))
    (if (i32.lt_s (i32.const 5) (local.get $x)) (then
      unreachable
    )))

  (start $start)
)

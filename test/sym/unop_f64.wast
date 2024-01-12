(module
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $x f64)
    (local $y f32)
    (local $i i32)
    (local $j i64)

    (local.set $x (call $f64_symbol))
    (local.set $y (call $f32_symbol))
    (local.set $i (call $i32_symbol))
    (local.set $j (call $i64_symbol))

    (f64.eq (f64.const 42) (f64.reinterpret_i64 (local.get $j)))
    (if (then unreachable))

    (f64.eq (f64.ceil (local.get $x)) (f64.const 123))
    (if (then unreachable))

    (f64.eq (f64.floor (local.get $x)) (f64.const 123))
    (if (then unreachable))

    (f64.ge (f64.abs (local.get $x)) (f64.const 42))
    (if (then unreachable))

    (f64.le (f64.neg (local.get $x)) (f64.const 42))
    (if (then unreachable))

    (f64.eq (f64.nearest (local.get $x)) (f64.const 123))
    (if (then unreachable))

    (f64.eq (f64.promote_f32 (local.get $y)) (f64.const 2))
    (if (then unreachable))

    (f64.eq (f64.convert_i32_s (local.get $i)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.convert_i32_u (local.get $i)) (f64.const 1))
    (if (then unreachable))

    (f64.eq (f64.convert_i64_s (local.get $j)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.convert_i64_u (local.get $j)) (f64.const 1))
    (if (then unreachable))
  )

  (start $start)
)

(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $i32 i32)
    (local $i64 i64)
    (local $f32 f32)
    (local $f64 f64)

    (local.set $i32 (call $i32_symbol))
    (local.set $i64 (call $i64_symbol))
    (local.set $f32 (call $f32_symbol))
    (local.set $f64 (call $f64_symbol))

    (f64.eq (f64.const 42) (f64.reinterpret_i64 (local.get $i64)))
    (if (then unreachable))

    (f64.eq (f64.ceil (local.get $f64)) (f64.const 123))
    (if (then unreachable))

    (f64.eq (f64.floor (local.get $f64)) (f64.const 123))
    (if (then unreachable))

    (f64.ge (f64.abs (local.get $f64)) (f64.const 42))
    (if (then unreachable))

    (f64.le (f64.neg (local.get $f64)) (f64.const 42))
    (if (then unreachable))

    (f64.eq (f64.trunc (local.get $f64)) (f64.const 123))
    (if (then unreachable))

    ;; very long computation
    ;; (f64.eq (f64.sqrt (local.get $f64)) (f64.const 0))
    ;; (if (then unreachable))

    (f64.eq (f64.nearest (local.get $f64)) (f64.const 123))
    (if (then unreachable))

    (f64.eq (f64.promote_f32 (local.get $f32)) (f64.const 2))
    (if (then unreachable))

    (f64.eq (f64.convert_i32_s (local.get $i32)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.convert_i32_u (local.get $i32)) (f64.const 1))
    (if (then unreachable))

    (f64.eq (f64.convert_i64_s (local.get $i64)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.convert_i64_u (local.get $i64)) (f64.const 1))
    (if (then unreachable))
  )

  (start $start)
)

(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $x f32)
    (local $y f64)
    (local $i i32)
    (local $j i64)

    (local.set $x (call $f32_symbol))
    (local.set $y (call $f64_symbol))
    (local.set $i (call $i32_symbol))
    (local.set $j (call $i64_symbol))

    (f32.ge (f32.abs (local.get $x)) (f32.const 42))
    (if (then unreachable))

    (f32.le (f32.neg (local.get $x)) (f32.const 42))
    (if (then unreachable))

    (f32.eq (f32.demote_f64 (local.get $y)) (f32.const 2))
    (if (then unreachable))

    (f32.eq (f32.convert_i32_s (local.get $i)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.convert_i32_u (local.get $i)) (f32.const 1))
    (if (then unreachable))

    (f32.eq (f32.convert_i64_s (local.get $j)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.convert_i64_u (local.get $j)) (f32.const 1))
    (if (then unreachable))

    ;; File "lib/z3_mappings.ml", line 408, characters 32-38: Assertion failed
    ;; (f32.ge (f32.ceil (local.get $x)) (f32.const 42))
    ;; (if (then unreachable))
    ;; (f32.le (f32.floor (local.get $x)) (f32.const 42))
    ;; (if (then unreachable))
  )

  (start $start)
)

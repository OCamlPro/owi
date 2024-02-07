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

    (f32.eq (f32.const 42) (f32.reinterpret_i32 (local.get $i32)))
    (if (then unreachable))

    (f32.eq (f32.ceil (local.get $f32)) (f32.const 123))
    (if (then unreachable))

    (f32.eq (f32.floor (local.get $f32)) (f32.const 123))
    (if (then unreachable))

    (f32.ge (f32.abs (local.get $f32)) (f32.const 42))
    (if (then unreachable))

    (f32.le (f32.neg (local.get $f32)) (f32.const 42))
    (if (then unreachable))

    ;; owi: internal error, uncaught exception:
    ;;  File "src/symbolic_value.ml", line 420, characters 18-24: Assertion failed
    ;; (f32.eq (f32.trunc (local.get $f32)) (f32.const 123))
    ;; (if (then unreachable))

    ;; very long computation
    ;; (f32.eq (f32.sqrt (local.get $f32)) (f32.const 0))
    ;; (if (then unreachable))

    (f32.eq (f32.nearest (local.get $f32)) (f32.const 123))
    (if (then unreachable))

    (f32.eq (f32.demote_f64 (local.get $f64)) (f32.const 2))
    (if (then unreachable))

    (f32.eq (f32.convert_i32_s (local.get $i32)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.convert_i32_u (local.get $i32)) (f32.const 1))
    (if (then unreachable))

    (f32.eq (f32.convert_i64_s (local.get $i64)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.convert_i64_u (local.get $i64)) (f32.const 1))
    (if (then unreachable))
  )

  (start $start)
)

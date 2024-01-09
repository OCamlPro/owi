(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $x f32)

    (local.set $x (call $f32_symbol))

    (f32.ge (f32.abs (local.get $x)) (f32.const 42))
    (if (then unreachable))

    (f32.le (f32.neg (local.get $x)) (f32.const 42))
    (if (then unreachable))

    ;; File "lib/z3_mappings.ml", line 408, characters 32-38: Assertion failed
    ;; (f32.ge (f32.ceil (local.get $x)) (f32.const 42))
    ;; (if (then unreachable))
    ;; (f32.le (f32.floor (local.get $x)) (f32.const 42))
    ;; (if (then unreachable))
  )

  (start $start)
)

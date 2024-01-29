(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume_positive_i32" (func $assume_positive_i32 (param i32)))
  (import "symbolic" "assert" (func $assert_i32 (param i32)))

  (func $start
    (local $x i32)
    (local $y i32)

    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))

    (call $assume_positive_i32 (local.get $x))  ;; x >= 0
    (call $assume_positive_i32 (local.get $y))  ;; y >= 0
    (call $assert_i32                           ;; x * y >= 0 (false when overflow)
      (i32.ge_s (i32.mul (local.get $x) (local.get $y)) (i32.const 0)))
  )

  (start $start)
)

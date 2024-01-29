(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume_positive_i32" (func $assume_positive_i32 (param i32)))
  (import "symbolic" "assert" (func $assert_i32 (param i32)))

  (func $start
    (local $x i32)
    (local $y i32)

    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))

    (call $assume_positive_i32 (local.get $x))  ;; 0 <= x
    (call $assume_positive_i32 (local.get $y))  ;; 0 <= y
    (call $assert_i32                           ;; 0 <= x + y (false when overflow)
      (i32.le_s (i32.const 0) (i32.add (local.get $x) (local.get $y))))
  )

  (start $start)
)

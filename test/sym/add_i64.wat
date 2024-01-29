(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "assume" (func $assume_i32 (param i32)))
  (import "symbolic" "assert" (func $assert_i32 (param i32)))

  (func $start
    (local $x i64)
    (local $y i64)

    (local.set $x (call $i64_symbol))
    (local.set $y (call $i64_symbol))

    (call $assume_i32
      (i64.le_s (i64.const 0) (local.get $x)))    ;; 0 <= x
    (call $assume_i32
      (i64.le_s (i64.const 0) (local.get $y)))    ;; 0 <= y
    (call $assert_i32                             ;; 0 <= x + y (false when overflow)
      (i64.le_s (i64.const 0) (i64.add (local.get $x) (local.get $y))))
  )

  (start $start)
)

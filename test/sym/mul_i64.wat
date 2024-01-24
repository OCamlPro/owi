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
      (i64.ge_s (local.get $x) (i64.const 0)))  ;; y >= 0
    (call $assume_i32
      (i64.ge_s (local.get $y) (i64.const 0)))  ;; y >= 0
    (call $assert_i32                           ;; x * y >= 0
      (i64.ge_s (i64.mul (local.get $x) (local.get $y)) (i64.const 0)))
  )

  (start $start)
)

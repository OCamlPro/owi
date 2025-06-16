(module
  (import "owi" "i64_symbol" (func $i64_symbol (result i64)))
  (import "owi" "assume" (func $assume (param i32)))
  (import "owi" "assert" (func $assert (param i32)))

  (func $start
    (local $x i64)
    (local $y i64)

    (local.set $x (call $i64_symbol))
    (local.set $y (call $i64_symbol))

    (call $assume
      (i64.ge_s (local.get $x) (i64.const 0)))  ;; x >= 0
    (call $assume
      (i64.ge_s (local.get $y) (i64.const 0)))  ;; y >= 0
    (call $assert                           ;; x * y >= 0 (false when overflow)
      (i64.ge_s (i64.mul (local.get $x) (local.get $y)) (i64.const 0)))
  )

  (start $start)
)

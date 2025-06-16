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
      (i64.le_s (i64.const 0) (local.get $x)))    ;; 0 <= x
    (call $assume
      (i64.le_s (i64.const 0) (local.get $y)))    ;; 0 <= y
    (call $assert                             ;; 0 <= x + y (false when overflow)
      (i64.le_s (i64.const 0) (i64.add (local.get $x) (local.get $y))))
  )

  (start $start)
)

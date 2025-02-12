(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $assume (param i32)))
  (import "symbolic" "assert" (func $assert (param i32)))

  (func $start
    (local $x i32)
    (local $y i32)

    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))

    (call $assume (i32.ge_s  (local.get $x) (i32.const 0)))  ;; x >= 0
    (call $assume (i32.ge_s  (local.get $y) (i32.const 0)))  ;; y >= 0
    (call $assert                           ;; 0 <= x + y (false when overflow)
      (i32.le_s (i32.const 0) (i32.add (local.get $x) (local.get $y))))
  )

  (start $start)
)

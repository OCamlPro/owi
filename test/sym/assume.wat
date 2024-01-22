(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "assume" (func $assume_i32 (param i32)))
  (import "symbolic" "assume_positive_i32" (func $positive_i32 (param i32)))
  (import "symbolic" "assert" (func $assert_i32 (param i32)))

  (func $start (local $x i32) (local $y i32)

    (local.set $x (call $i32_symbol))
    (local.set $y (call $i32_symbol))

    (call $positive_i32 (local.get $x)) ;; x >= 0
    (call $assume_i32 (i32.gt_s (local.get $y) (i32.const 0))) ;; y > 0

    ;; if x + y <= 0
    (if (i32.le_s (i32.add (local.get $x) (local.get $y)) (i32.const 0))
      ;; possible if they overflow
      (then unreachable))

    ;; otherwise (x + y > 0)
    (call $assert_i32 (i32.gt_s (i32.add (local.get $x) (local.get $y)) (i32.const 0)))
  )

  (start $start))

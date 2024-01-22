(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (global $x (mut i32) (i32.const 0))
  (global $y (mut i64) (i64.const 0))
  (global $i (mut f32) (f32.const 0))
  (global $j (mut f64) (f64.const 0))

  (func $start
    (global.set $x (call $i32_symbol))
    (global.set $y (call $i64_symbol))
    (global.set $i (call $f32_symbol))
    (global.set $j (call $f64_symbol))

    (if (i32.lt_s (i32.const 5) (global.get $x))
      (then unreachable))

    (if (i64.gt_s (i64.const 5) (global.get $y))
      (then unreachable))

    (if (f32.lt (f32.const 5) (global.get $i))
      (then unreachable))

    (if (f64.gt (f64.const 5) (global.get $j))
      (then unreachable)))

  (start $start)
)

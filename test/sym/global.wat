(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (global $x1 (mut i32) (i32.const 0))
  (global $x2 (mut i32) (i32.const 0))

  (global $y1 (mut i64) (i64.const 0))
  (global $y2 (mut i64) (i64.const 0))

  (global $i1 (mut f32) (f32.const 0))
  (global $i2 (mut f32) (f32.const 0))

  (global $j1 (mut f64) (f64.const 0))
  (global $j2 (mut f64) (f64.const 0))

  (func $i32 (param $x i32) (param $y i32)
    (if (i32.lt_s (local.get $x) (local.get $y))
      (then unreachable))
  )

  (func $i64 (param $x i64) (param $y i64)
    (if (i64.gt_s (local.get $x) (local.get $y))
      (then unreachable))
  )

  (func $f32 (param $x f32) (param $y f32)
    (if (f32.lt (local.get $x) (local.get $y))
      (then unreachable))
  )

  (func $f64 (param $x f64) (param $y f64)
    (if (f64.gt (local.get $x) (local.get $y))
      (then unreachable))
  )

  (func $start
    (global.set $x1 (call $i32_symbol))
    (global.set $x2 (call $i32_symbol))
    (call $i32 (global.get $x1) (global.get $x2))
    ;; (call $i32 (global.get $x1) (i32.const 0))
    ;; (call $i32 (i32.const 0) (global.get $x2))

    (global.set $y1 (call $i64_symbol))
    (global.set $y2 (call $i64_symbol))
    (call $i64 (global.get $y1) (global.get $y2))

    (global.set $i1 (call $f32_symbol))
    (global.set $i2 (call $f32_symbol))
    (call $f32 (global.get $i1) (global.get $i2))

    (global.set $j1 (call $f64_symbol))
    (global.set $j2 (call $f64_symbol))
    (call $f64 (global.get $j1) (global.get $j2))
  )

  (start $start)
)

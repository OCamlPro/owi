(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (global $w (mut i32) (i32.const 0))
  (global $x (mut i64) (i64.const 0))
  (global $y (mut f32) (f32.const 0))
  (global $z (mut f64) (f64.const 0))

  (func $i32 (param $a i32)
    (if (i32.eqz (local.get $a))
      (then unreachable))
  )

  (func $i64 (param $a i64)
    (if (i64.eqz (local.get $a))
      (then unreachable))
  )

  (func $f32 (param $a f32)
    (if (f32.eq (local.get $a) (f32.const 0))
      (then unreachable))
  )

  (func $f64 (param $a f64)
    (if (f64.eq (local.get $a) (f64.const 0))
      (then unreachable))
  )

  (func $start
    ;; objective: check that interactions with globals are ok

    (global.set $w (call $i32_symbol))
    (global.set $x (call $i64_symbol))
    (global.set $y (call $f32_symbol))
    (global.set $z (call $f64_symbol))

    ;; 'unreachable' when argument is equal to 0
    ;; in global.t, only one model symbol is equal to 0
    (call $i32 (global.get $w))
    (call $i64 (global.get $x)) ;; (symbol_0) w != 0
    (call $f32 (global.get $y)) ;; (symbol_0) w != 0 AND (symbol_1) x != 0
    (call $f64 (global.get $z)) ;; (symbol_0) w != 0 AND (symbol_1) x != 0 AND (symbol_2) y != 0
  )

  (start $start)
)

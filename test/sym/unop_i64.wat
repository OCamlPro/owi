(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $i32 i32)
    (local $i64 i64)
    (local $f32 f32)
    (local $f64 f64)

    (local.set $i32 (call $i32_symbol))
    (local.set $i64 (call $i64_symbol))
    (local.set $f32 (call $f32_symbol))
    (local.set $f64 (call $f64_symbol))

    (i64.eqz (local.get $i64))
    (if (then unreachable))

    (i64.eq (i64.const 10) (i64.extend8_s (local.get $i64)))
    (if (then unreachable))

    (i64.eq (i64.const 20) (i64.extend16_s (local.get $i64)))
    (if (then unreachable))

    (i64.eq (i64.const 30) (i64.extend32_s (local.get $i64)))
    (if (then unreachable))

    (i64.eq (i64.const 42) (i64.reinterpret_f64 (local.get $f64)))
    (if (then unreachable))

    (i64.eq (i64.const 42) (i64.extend_i32_s (local.get $i32)))
    (if (then unreachable))

    (i64.eq (i64.const 4242) (i64.extend_i32_u (local.get $i32)))
    (if (then unreachable))

    (i64.ge_u (i64.const 0) (i64.trunc_f32_u (local.get $f32)))
    (if (then unreachable))
    
    (i64.ge_s (i64.const 0) (i64.trunc_f32_s (local.get $f32)))
    (if (then unreachable))

    (i64.le_u (i64.const 100) (i64.trunc_f32_u (local.get $f32)))
    (if (then unreachable))
    
    (i64.le_s (i64.const 100) (i64.trunc_f32_s (local.get $f32)))
    (if (then unreachable))

    (i64.lt_u (i64.const 0) (i64.trunc_f32_u (local.get $f32)))
    (if (then unreachable))

    (i64.lt_s (i64.const 0) (i64.trunc_f32_s (local.get $f32)))
    (if (then unreachable))

    (i64.gt_u (i64.const 100) (i64.trunc_f32_u (local.get $f32)))
    (if (then unreachable))

    (i64.gt_s (i64.const 100) (i64.trunc_f32_s (local.get $f32)))
    (if (then unreachable))

    (i64.ge_u (i64.const 0) (i64.trunc_f64_u (local.get $f64)))
    (if (then unreachable))
    
    (i64.ge_s (i64.const 0) (i64.trunc_f64_s (local.get $f64)))
    (if (then unreachable))

    (i64.le_u (i64.const 100) (i64.trunc_f64_u (local.get $f64)))
    (if (then unreachable))
    
    (i64.le_s (i64.const 100) (i64.trunc_f64_s (local.get $f64)))
    (if (then unreachable))

    (i64.lt_u (i64.const 0) (i64.trunc_f64_u (local.get $f64)))
    (if (then unreachable))

    (i64.lt_s (i64.const 0) (i64.trunc_f64_s (local.get $f64)))
    (if (then unreachable))

    (i64.gt_u (i64.const 100) (i64.trunc_f64_u (local.get $f64)))
    (if (then unreachable))

    (i64.gt_s (i64.const 100) (i64.trunc_f64_s (local.get $f64)))
    (if (then unreachable))

    (i64.le_s (i64.const -42) (i64.clz (local.get $i64)))
    (if (then unreachable))

    (i64.le_s (i64.const -42) (i64.ctz (local.get $i64)))
    (if (then unreachable))

    (i64.le_s (i64.const -42) (i64.popcnt (local.get $i64)))
    (if (then unreachable))
  )

  (start $start)
)

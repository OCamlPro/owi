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

    (i32.eqz (local.get $i32))
    (if (then unreachable))

    (i32.eq (i32.const 42) (i32.wrap_i64 (local.get $i64)))
    (if (then unreachable))

    (i32.eq (i32.const 42) (i32.reinterpret_f32 (local.get $f32)))
    (if (then unreachable))

    (i32.eq (i32.const 21) (i32.extend8_s (local.get $i32)))
    (if (then unreachable))

    (i32.eq (i32.const 24) (i32.extend16_s (local.get $i32)))
    (if (then unreachable))

    (i32.ge_u (i32.const 0) (i32.trunc_f32_u (local.get $f32)))
    (if (then unreachable))
    
    (i32.ge_s (i32.const 0) (i32.trunc_f32_s (local.get $f32)))
    (if (then unreachable))

    (i32.le_u (i32.const 100) (i32.trunc_f32_u (local.get $f32)))
    (if (then unreachable))
    
    (i32.le_s (i32.const 100) (i32.trunc_f32_s (local.get $f32)))
    (if (then unreachable))

    (i32.lt_u (i32.const 0) (i32.trunc_f32_u (local.get $f32)))
    (if (then unreachable))

    (i32.lt_s (i32.const 0) (i32.trunc_f32_s (local.get $f32)))
    (if (then unreachable))

    (i32.gt_u (i32.const 100) (i32.trunc_f32_u (local.get $f32)))
    (if (then unreachable))

    (i32.gt_s (i32.const 100) (i32.trunc_f32_s (local.get $f32)))
    (if (then unreachable))

    (i32.ge_u (i32.const 0) (i32.trunc_f64_u (local.get $f64)))
    (if (then unreachable))
    
    (i32.ge_s (i32.const 0) (i32.trunc_f64_s (local.get $f64)))
    (if (then unreachable))

    (i32.le_u (i32.const 100) (i32.trunc_f64_u (local.get $f64)))
    (if (then unreachable))
    
    (i32.le_s (i32.const 100) (i32.trunc_f64_s (local.get $f64)))
    (if (then unreachable))

    (i32.lt_u (i32.const 0) (i32.trunc_f64_u (local.get $f64)))
    (if (then unreachable))

    (i32.lt_s (i32.const 0) (i32.trunc_f64_s (local.get $f64)))
    (if (then unreachable))

    (i32.gt_u (i32.const 100) (i32.trunc_f64_u (local.get $f64)))
    (if (then unreachable))

    (i32.gt_s (i32.const 100) (i32.trunc_f64_s (local.get $f64)))
    (if (then unreachable))

    (i32.ge_s (i32.const 42) (i32.clz (local.get $i32)))
    (if (then unreachable))

    (i32.ge_s (i32.const 42) (i32.ctz (local.get $i32)))
    (if (then unreachable))

    (i32.ge_s (i32.const 42) (i32.popcnt (local.get $i32)))
    (if (then unreachable))
  )

  (start $start)
)

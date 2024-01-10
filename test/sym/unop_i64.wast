(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $x i64)
    (local $i f32)
    (local $j f64)

    (local.set $x (call $i64_symbol))
    (local.set $i (call $f32_symbol))
    (local.set $j (call $f64_symbol))

    (i64.eqz (local.get $x))
    (if (then unreachable))

    (i64.ge_u (i64.const 0) (i64.trunc_f32_u (local.get $i)))
    (if (then unreachable))
    
    (i64.ge_s (i64.const 0) (i64.trunc_f32_s (local.get $i)))
    (if (then unreachable))

    (i64.le_u (i64.const 100) (i64.trunc_f32_u (local.get $i)))
    (if (then unreachable))
    
    (i64.le_s (i64.const 100) (i64.trunc_f32_s (local.get $i)))
    (if (then unreachable))

    (i64.lt_u (i64.const 0) (i64.trunc_f32_u (local.get $i)))
    (if (then unreachable))

    (i64.lt_s (i64.const 0) (i64.trunc_f32_s (local.get $i)))
    (if (then unreachable))

    (i64.gt_u (i64.const 100) (i64.trunc_f32_u (local.get $i)))
    (if (then unreachable))

    (i64.gt_s (i64.const 100) (i64.trunc_f32_s (local.get $i)))
    (if (then unreachable))

    (i64.ge_u (i64.const 0) (i64.trunc_f64_u (local.get $j)))
    (if (then unreachable))
    
    (i64.ge_s (i64.const 0) (i64.trunc_f64_s (local.get $j)))
    (if (then unreachable))

    (i64.le_u (i64.const 100) (i64.trunc_f64_u (local.get $j)))
    (if (then unreachable))
    
    (i64.le_s (i64.const 100) (i64.trunc_f64_s (local.get $j)))
    (if (then unreachable))

    (i64.lt_u (i64.const 0) (i64.trunc_f64_u (local.get $j)))
    (if (then unreachable))

    (i64.lt_s (i64.const 0) (i64.trunc_f64_s (local.get $j)))
    (if (then unreachable))

    (i64.gt_u (i64.const 100) (i64.trunc_f64_u (local.get $j)))
    (if (then unreachable))

    (i64.gt_s (i64.const 100) (i64.trunc_f64_s (local.get $j)))
    (if (then unreachable))
  )

  (start $start)
)

(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $x i32)
    (local $i f32)
    (local $j f64)

    (local.set $x (call $i32_symbol))
    (local.set $i (call $f32_symbol))
    (local.set $j (call $f64_symbol))

    (i32.eqz (local.get $x))
    (if (then unreachable))

    (i32.ge_u (i32.const 0) (i32.trunc_f32_u (local.get $i)))
    (if (then unreachable))
    
    (i32.ge_s (i32.const 0) (i32.trunc_f32_s (local.get $i)))
    (if (then unreachable))

    (i32.le_u (i32.const 100) (i32.trunc_f32_u (local.get $i)))
    (if (then unreachable))
    
    (i32.le_s (i32.const 100) (i32.trunc_f32_s (local.get $i)))
    (if (then unreachable))

    (i32.lt_u (i32.const 0) (i32.trunc_f32_u (local.get $i)))
    (if (then unreachable))

    (i32.lt_s (i32.const 0) (i32.trunc_f32_s (local.get $i)))
    (if (then unreachable))

    (i32.gt_u (i32.const 100) (i32.trunc_f32_u (local.get $i)))
    (if (then unreachable))

    (i32.gt_s (i32.const 100) (i32.trunc_f32_s (local.get $i)))
    (if (then unreachable))

    (i32.ge_u (i32.const 0) (i32.trunc_f64_u (local.get $j)))
    (if (then unreachable))
    
    (i32.ge_s (i32.const 0) (i32.trunc_f64_s (local.get $j)))
    (if (then unreachable))

    (i32.le_u (i32.const 100) (i32.trunc_f64_u (local.get $j)))
    (if (then unreachable))
    
    (i32.le_s (i32.const 100) (i32.trunc_f64_s (local.get $j)))
    (if (then unreachable))

    (i32.lt_u (i32.const 0) (i32.trunc_f64_u (local.get $j)))
    (if (then unreachable))

    (i32.lt_s (i32.const 0) (i32.trunc_f64_s (local.get $j)))
    (if (then unreachable))

    (i32.gt_u (i32.const 100) (i32.trunc_f64_u (local.get $j)))
    (if (then unreachable))

    (i32.gt_s (i32.const 100) (i32.trunc_f64_s (local.get $j)))
    (if (then unreachable))

    ;; Memo "lib/z3_mappings.ml" (to be implemented)
    ;; Clz not supported yet
    ;; (i32.ge_s (i32.const 2) (i32.clz (local.get $x)) )
    ;; (if (then unreachable))
    ;; Ctz: TODO
    ;; (i32.ge_s (i32.const 2) (i32.ctz (local.get $x)) )
    ;; (if (then unreachable))
    ;; Popcnt: TODO
    ;; (i32.ge_s (i32.const 2) (i32.popcnt (local.get $x)) )
    ;; (if (then unreachable))

  )

  (start $start)
)

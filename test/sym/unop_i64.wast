(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $x i64)
    (local $y i32)
    (local $i f32)
    (local $j f64)

    (local.set $x (call $i64_symbol))
    (local.set $y (call $i32_symbol))
    (local.set $i (call $f32_symbol))
    (local.set $j (call $f64_symbol))

    (i64.eqz (local.get $x))
    (if (then unreachable))

    ;;  owi: internal error, uncaught exception:
    ;;    Z3.Error("Argument ((_ sign_extend 16) ((_ fp.to_ubv 64) roundTowardZero symbol_2)) at position 1 has sort
    ;;    (_ BitVec 80) it does not match declaration (declare-fun bvuge ((_ BitVec 64) (_ BitVec 64)) Bool)")
    ;; i64.extend8_s
    ;; i64.extend16_s
    ;; i64.extend32_s

    (i64.eq (i64.const 42) (i64.reinterpret_f64 (local.get $j)))
    (if (then unreachable))

    (i64.eq (i64.const 42) (i64.extend_i32_s (local.get $y)))
    (if (then unreachable))

    (i64.eq (i64.const 4242) (i64.extend_i32_u (local.get $y)))
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

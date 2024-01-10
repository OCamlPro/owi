(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $x i64)
    
    (local.set $x (call $i64_symbol))

    (i64.le_s (local.get $x) (i64.const -100))
    (if (then unreachable))
    (i64.lt_s (local.get $x) (i64.const -100))
    (if (then unreachable))

    (i64.ge_s (local.get $x) (i64.const 100))
    (if (then unreachable))
    (i64.gt_s (local.get $x) (i64.const 100))
    (if (then unreachable))

    (i64.eq (local.get $x) (i64.const 50))
    (if (then unreachable))

    (i64.add (local.get $x) (i64.const 42))
    i64.eqz
    (if (then unreachable))

    (i64.sub (local.get $x) (i64.const 42))
    i64.eqz
    (if (then unreachable))

    (i64.mul (local.get $x) (i64.const 42))
    i64.eqz
    (if (then unreachable))

    (i64.div_u (local.get $x) (i64.const 42))
    i64.eqz
    (if (then unreachable))

    (i64.rem_u (i64.const 42) (local.get $x))
    i64.eqz
    (if (then unreachable))

    (i64.div_s (local.get $x) (i64.const 42))
    i64.eqz
    (if (then unreachable))

    (i64.rem_s (i64.const 42) (local.get $x))
    i64.eqz
    (if (then unreachable))

    (i64.ne (local.get $x) (i64.const 0))
    (if (then unreachable)))

  (start $start)
)

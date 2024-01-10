(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $x i32)

    (local.set $x (call $i32_symbol))

    (i32.le_s (local.get $x) (i32.const -100))
    (if (then unreachable))
    ;; (> -100)
    (i32.lt_s (local.get $x) (i32.const -100))
    (if (then unreachable))
    ;; (> -100)

    (i32.ge_s (local.get $x) (i32.const 100))
    (if (then unreachable))
    ;; (> -100) and (< 100)
    (i32.gt_s (local.get $x) (i32.const 100))
    (if (then unreachable))
    ;; (> -100) and (< 100)

    (i32.eq (local.get $x) (i32.const 50))
    (if (then unreachable))
    ;; (> -100) and (< 100) and (!= 50)

    (i32.add (local.get $x) (i32.const 42))
    i32.eqz
    (if (then unreachable))

    (i32.sub (local.get $x) (i32.const 42))
    i32.eqz
    (if (then unreachable))

    (i32.mul (local.get $x) (i32.const 42))
    i32.eqz
    (if (then unreachable))

    (i32.div_u (local.get $x) (i32.const 42))
    i32.eqz
    (if (then unreachable))

    (i32.rem_u (i32.const 42) (local.get $x))
    i32.eqz
    (if (then unreachable))

    (i32.div_s (local.get $x) (i32.const 42))
    i32.eqz
    (if (then unreachable))

    (i32.rem_s (i32.const 42) (local.get $x))
    i32.eqz
    (if (then unreachable))

    (i32.ne (local.get $x) (i32.const 0))
    (if (then unreachable)))

  (start $start)
)

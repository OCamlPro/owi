(module
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $x f64)
    (local.set $x (call $f64_symbol))

    (f64.le (local.get $x) (f64.const -100))
    (if (then unreachable))

    (f64.lt (local.get $x) (f64.const -100))
    (if (then unreachable))

    (f64.ge (local.get $x) (f64.const 100))
    (if (then unreachable))

    (f64.gt (local.get $x) (f64.const 100))
    (if (then unreachable))

    (f64.eq (f64.add (local.get $x) (f64.const 42)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.sub (local.get $x) (f64.const 42)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.mul (local.get $x) (f64.const 42)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.div (local.get $x) (f64.const 42)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.min (local.get $x) (f64.const 42)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.max (local.get $x) (f64.const 42)) (f64.const 0))
    (if (then unreachable))

    (f64.eq (f64.copysign (local.get $x) (f64.const 42)) (f64.const 0))
    (if (then unreachable))

    (f64.ne (local.get $x) (f64.const 42))
    (if (then unreachable)))

  (start $start)
)

(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $x i64)
    
    (local.set $x (call $i64_symbol))

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

    (i64.div_s (local.get $x) (i64.const 42))
    i64.eqz
    (if (then unreachable)))

  (start $start)
)

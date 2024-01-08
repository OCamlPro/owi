(module
  (import "symbolic" "i64_symbol" (func $i64_symbol (result i64)))

  (func $start
    (local $x i64)
    (local $res i64)
    
    (local.set $x (call $i64_symbol))

    (local.set $res (i64.add (local.get $x) (i64.const 42)))
    (if (i64.eqz (local.get $res))
      (then unreachable))

    (local.set $res (i64.sub (local.get $x) (i64.const 42)))
    (if (i64.eqz (local.get $res))
      (then unreachable))

    (local.set $res (i64.mul (local.get $x) (i64.const 42)))
    (if (i64.eqz (local.get $res))
      (then unreachable))

    (local.set $res (i64.div_u (local.get $x) (i64.const 42)))
    (if (i64.eqz (local.get $res))
      (then unreachable))

    (local.set $res (i64.div_s (local.get $x) (i64.const 42)))
    (if (i64.eqz (local.get $res))
      (then unreachable)))

  (start $start)
)

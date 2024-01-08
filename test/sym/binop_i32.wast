(module
  (import "symbolic" "i32_symbol" (func $i32_symbol (result i32)))

  (func $start
    (local $x i32)
    (local $res i32)
    
    (local.set $x (call $i32_symbol))

    (local.set $res (i32.add (local.get $x) (i32.const 42)))
    (if (i32.eqz (local.get $res))
      (then unreachable))

    (local.set $res (i32.sub (local.get $x) (i32.const 42)))
    (if (i32.eqz (local.get $res))
      (then unreachable))

    (local.set $res (i32.mul (local.get $x) (i32.const 42)))
    (if (i32.eqz (local.get $res))
      (then unreachable))

    (local.set $res (i32.div_u (local.get $x) (i32.const 42)))
    (if (i32.eqz (local.get $res))
      (then unreachable))

    (local.set $res (i32.div_s (local.get $x) (i32.const 42)))
    (if (i32.eqz (local.get $res))
      (then unreachable)))

  (start $start)
)

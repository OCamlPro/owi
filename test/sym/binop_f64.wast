(module
  (import "symbolic" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $x f64)
    (local $res f64)
    
    (local.set $x (call $f64_symbol))

    (local.set $res (f64.add (local.get $x) (f64.const 42)))
    (if (f64.eq (local.get $res) (f64.const 0))
      (then unreachable))

    (local.set $res (f64.sub (local.get $x) (f64.const 42)))
    (if (f64.eq (local.get $res) (f64.const 0))
      (then unreachable))

    (local.set $res (f64.mul (local.get $x) (f64.const 42)))
    (if (f64.eq (local.get $res) (f64.const 0))
      (then unreachable))

    (local.set $res (f64.div (local.get $x) (f64.const 42)))
    (if (f64.eq (local.get $res) (f64.const 0))
      (then unreachable)))

  (start $start)
)

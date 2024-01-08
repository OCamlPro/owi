(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $x f32)
    (local $res f32)
    
    (local.set $x (call $f32_symbol))

    (local.set $res (f32.add (local.get $x) (f32.const 42)))
    (if (f32.eq (local.get $res) (f32.const 0))
      (then unreachable))

    (local.set $res (f32.sub (local.get $x) (f32.const 42)))
    (if (f32.eq (local.get $res) (f32.const 0))
      (then unreachable))

    (local.set $res (f32.mul (local.get $x) (f32.const 42)))
    (if (f32.eq (local.get $res) (f32.const 0))
      (then unreachable))

    (local.set $res (f32.div (local.get $x) (f32.const 42)))
    (if (f32.eq (local.get $res) (f32.const 0))
      (then unreachable)))

  (start $start)
)

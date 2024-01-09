(module
  (import "symbolic" "f32_symbol" (func $f32_symbol (result f32)))

  (func $start
    (local $x f32)
    
    (local.set $x (call $f32_symbol))

    (f32.eq (f32.add (local.get $x) (f32.const 42)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.sub (local.get $x) (f32.const 42)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.mul (local.get $x) (f32.const 42)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.div (local.get $x) (f32.const 42)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.min (local.get $x) (f32.const 42)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.max (local.get $x) (f32.const 42)) (f32.const 0))
    (if (then unreachable))

    (f32.eq (f32.copysign (local.get $x) (f32.const 42)) (f32.const 0))
    (if (then unreachable)))

  (start $start)
)

(module
  (import "owi" "f64_symbol" (func $f64_symbol (result f64)))

  (func $start
    (local $f64 f64)
    (local.set $f64 (call $f64_symbol))

    (f32.eq (f32.demote_f64 (local.get $f64)) (f32.const 0))
    (if (then unreachable))
  )

  (start $start)
)

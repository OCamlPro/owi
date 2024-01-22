(module
  (func $start
    f32.const 0
    f32.const 20
    f32.const 21
    f32.add
    f32.const 1
    f32.add
    f32.add
    drop

    f64.const 1
    f64.const 7
    f64.const 3
    f64.mul
    f64.const 2
    f64.mul
    f64.mul
    drop
  )
  (start $start)
)

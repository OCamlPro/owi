(module 
  (func $start
    f32.const 4.
    f32.const 100.
    f32.sqrt
    f32.add
    i32.trunc_f32_s
    f32.convert_i32_s
    f64.promote_f32
    f64.const 4294967296.
    f64.add
    f64.const 4294967295.
    f64.sub
    return
  )
  (start $start)
)

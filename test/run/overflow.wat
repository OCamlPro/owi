(module
  (func $f
    f64.const 1.79769313486e+308
    f64.const -100
    f64.mul
    i32.trunc_f64_s
    drop
  )
  (start $f)
)

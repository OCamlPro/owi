(module
  (func (export "f32") (result i32)
    (i32.eqz (i32.const 42))
    (i32.eqz (i32.const 0))
    i32.add
  )
)

(assert_return (invoke "f32") (i32.const 1))

(module
  (func (export "f64") (result i32)
    (i64.eqz (i64.const 42))
    (i64.eqz (i64.const 0))
    i32.add
  )
)

(assert_return (invoke "f64") (i32.const 1))

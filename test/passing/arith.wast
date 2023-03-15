(module
  (func $div0 (export "div0") (result i32)
    i32.const 42
    i32.const 0
    i32.div_u
  )
)

(assert_trap (invoke "div0") "integer divide by zero")

(module
  (func $div0 (export "div0") (result i32)
    i32.const 42
    i32.const 0
    i32.div_s
  )
)

(assert_trap (invoke "div0") "integer divide by zero")

(module
  (func $div0 (export "div0") (result i64)
    i64.const 42
    i64.const 0
    i64.div_u
  )
)

(assert_trap (invoke "div0") "integer divide by zero")

(module
  (func $div0 (export "div0") (result i64)
    i64.const 42
    i64.const 0
    i64.div_s
  )
)

(assert_trap (invoke "div0") "integer divide by zero")

(module
  (func $div0 (export "div0") (result f32)
    f32.const 42
    f32.const 0
    f32.div
  )
)

(assert_return (invoke "div0") (f32.const inf))

(module
  (func $div0 (export "div0") (result f64)
    f64.const 42
    f64.const 0
    f64.div
  )
)

(assert_return (invoke "div0") (f64.const inf))

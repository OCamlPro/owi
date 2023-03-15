(module
  (func $div0 (export "div0") (result i32)
    i32.const 0
    i32.const 42
    i32.div_u
  )
)

(assert_trap (invoke "div0") "integer divide by zero")

(module
  (func $f (export "42") (result i32)
    return (i32.const 42)
  )
)

(assert_return (invoke "42") (i32.const 42))

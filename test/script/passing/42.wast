(module
  (func $f (export "42") (result i32)
    i32.const 42
    return
  )
)

(assert_return (invoke "42") (i32.const 42))

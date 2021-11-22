(module
  (func $f (export "f")
    i32.const 0
    i32.const 1
    i32.const 2
  )
)

(assert_return (invoke "f") (i32.const 9))

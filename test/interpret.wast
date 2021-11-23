(module
  (func $f (export "f")
    i32.const 0
    return
  )
  (func $g (export "g")
    i32.const 256
    i32.clz
    return
  )
  (func $h (export "h")
    f32.const 9
    f32.sqrt
    return
  )
)

(assert_return (invoke "f") (i32.const 0))
(assert_return (invoke "g") (i32.const 23))
(assert_return (invoke "h") (f32.const 3))

(module
  (memory 1)
  (func $f (export "f") (result i32)
    (i32.store (i32.const 0) (i32.const 2))
    (i32.load (i32.const 0))
    return
  )
  (func $g (export "g") (result i32)
    (i32.store
      (i32.const 0)
      (i32.add
        (i32.load (i32.const 0))
        (i32.load (i32.const 0))))
    (i32.load (i32.const 0))
    return
  )
)

(assert_return (invoke "f") (i32.const 2))
(assert_return (invoke "g") (i32.const 4))
(assert_return (invoke "g") (i32.const 8))
(assert_return (invoke "g") (i32.const 16))
(assert_return (invoke "g") (i32.const 32))
(assert_return (invoke "g") (i32.const 64))
(assert_return (invoke "g") (i32.const 128))
(assert_return (invoke "g") (i32.const 256))
(assert_return (invoke "g") (i32.const 512))
(assert_return (invoke "g") (i32.const 1024))
(assert_return (invoke "g") (i32.const 2048))

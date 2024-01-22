(module
  (memory 0)
  (func (export "f") (result i32)
    i32.const -1
    memory.grow
  )
)

(assert_return (invoke "f") (i32.const -1))

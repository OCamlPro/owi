(module
  (memory 0)
  (func (export "f")
    i32.const -1
    i32.const -1
    i32.const -1
    memory.fill
  )
)

(assert_trap (invoke "f") "out of bounds memory access")

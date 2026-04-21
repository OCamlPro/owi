;; empty data
(module
  (memory 1)
  (data $p "")
  (data $a (memory 0) (i32.const 0) "")

  (func (export "init_passive") (param $len i32)
    (memory.init $p (i32.const 0) (i32.const 0) (local.get $len)))

  (func (export "init_active") (param $len i32)
    (memory.init $a (i32.const 0) (i32.const 0) (local.get $len)))
)

(assert_trap (invoke "init_passive" (i32.const 1)) "out of bounds memory access")
(assert_return (invoke "init_passive" (i32.const 0)))
(assert_trap (invoke "init_passive" (i32.const 1)) "out of bounds memory access")
(invoke "init_passive" (i32.const 0))
(assert_return (invoke "init_active" (i32.const 0)))
(assert_trap (invoke "init_active" (i32.const 1)) "out of bounds memory access")
(invoke "init_active" (i32.const 0))

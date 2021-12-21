;; Test globals

(module
  (global $a i32 (global.get $a))
  (func (export "get") (result i32) (global.get $a))
)

(assert_return (invoke "get") (i32.const 0))

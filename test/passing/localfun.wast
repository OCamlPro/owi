(module
  (type $signature (func (param i32) (result i32)))
  (func (export "f") (type $signature)
        (local $var i32)
        (local.get $var))
)

(assert_return (invoke "f" (i32.const 42)) (i32.const 0))

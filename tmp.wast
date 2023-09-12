(module
  (memory 1)
  (data (i32.const 0) "ABC\a7D") (data (i32.const 20) "WASM")

  (func (export "i64_load32_u") (param $i i64) (result i64)
	(i64.store32 (i32.const 8) (local.get $i))
	(i64.load32_u (i32.const 8))
  )
)

(assert_return (invoke "i64_load32_u" (i64.const -1)) (i64.const 4294967295))

  $ owi wasm of_wat block_type1.wat
  $ owi wasm to_wat block_type1.wasm
  (module
    (type (func))
    (type (func (param i32) (result i32)))
    (func
      i32.const 1
      (block (param i32) (result i32)
        i32.const 1
        i32.add)
      drop
    )
    (start 0)
  )
  $ owi wasm sym block_type1.wasm
  All OK!

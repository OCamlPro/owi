  $ owi wat2wasm symbolic.wat
  $ owi conc symbolic.wasm
  owi: [ERROR] Trap: unreachable
  Model:
   model {
    symbol symbol_1 i32 6
  }
  owi: [ERROR] Reached problem!
  [13]
  $ owi wasm2wat symbolic.wasm
  (module
    (import "owi" "i32_symbol" (func  (result i32)))
    (type (func (result i32)))
    (type (func))
    (func (local i32)
      call 0
      local.set 0
      i32.const 5
      local.get 0
      i32.lt_s
      (if
        (then
          unreachable
        )
      )
    )
    (start 1)
  )

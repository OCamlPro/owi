  $ owi wat2wasm loop.wat
  $ owi run loop.wasm --debug
  typechecking ...
  start function must have type [] -> []
  [32]
  $ owi wasm2wat loop.wasm
  (module
    
    (type (sub final  (func (param i32) (result i64))))
    
    (type (sub final  (func)))
    (func
      i32.const 3
      call 0
      drop
    )
    (func (param i32) (result i64) (local i32)
      i32.const 0
      local.set 1
      (loop (result i64)
        local.get 1
        i32.const 1
        i32.add
        local.tee 1
        local.get 0
        i32.ne
        (if
          (then
            br 1
          )
          (else
            nop
          )
        )
        i64.const 42)
    )
    (start 1)
  )

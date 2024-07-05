  $ owi wat2wasm func.wat
  $ owi run func.wasm --debug
  typechecking ...
  start function must have type [] -> []
  [32]
  $ owi wasm2wat func.wasm
  (module
    
    (type (sub final  (func (param f32) (result f32))))
    
    (type (sub final  (func)))
    (func
      f32.const 40.400_001_525_878_906
      call 0
      drop
    )
    (func (param f32) (result f32) (local f32)
      f32.const 2.019_999_980_926_513_7
      local.tee 1
      local.get 0
      f32.add
    )
    (start 1)
  )

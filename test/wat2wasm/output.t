  $ owi wat2wasm func.wat -o bar.wasm
  $ owi run bar.wasm -v
  owi: [INFO] Parsing time : 133μs
  owi: [INFO] typechecking ...
  owi: [INFO] Typechecking time : 18.5μs
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : f32.const 40.400_001_525_878_906
  owi: [INFO] stack         : [ f32.const 40.400_001_525_878_906 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : f32.const 2.019_999_980_926_513_7
  owi: [INFO] stack         : [ f32.const 2.019_999_980_926_513_7 ]
  owi: [INFO] running instr : local.tee 1
  owi: [INFO] stack         : [ f32.const 2.019_999_980_926_513_7 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ f32.const 40.400_001_525_878_906 ; f32.const 2.019_999_980_926_513_7 ]
  owi: [INFO] running instr : f32.add
  owi: [INFO] stack         : [ f32.const 42.420_001_983_642_578 ]
  owi: [INFO] running instr : drop

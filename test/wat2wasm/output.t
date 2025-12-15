  $ owi wat2wasm func.wat -o bar.wasm
  $ owi run bar.wasm -v
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 1 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : f32.const 40.400_001_525_878_906 (executed 0 times)
  owi: [INFO] stack         : [ f32.const 40.400_001_525_878_906 ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : f32.const 2.019_999_980_926_513_7 (executed 0 times)
  owi: [INFO] stack         : [ f32.const 2.019_999_980_926_513_7 ]
  owi: [INFO] running instr : local.tee 1 (executed 0 times)
  owi: [INFO] stack         : [ f32.const 2.019_999_980_926_513_7 ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ f32.const 40.400_001_525_878_906 ; f32.const 2.019_999_980_926_513_7 ]
  owi: [INFO] running instr : f32.add (executed 0 times)
  owi: [INFO] stack         : [ f32.const 42.420_001_983_642_578 ]
  owi: [INFO] running instr : drop (executed 0 times)

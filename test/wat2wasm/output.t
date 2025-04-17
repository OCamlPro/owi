  $ owi wat2wasm func.wat -o bar.wasm
  $ owi run bar.wasm --debug
  typechecking ...
  linking      ...
  interpreting ...
  stack         : [  ]
  running instr : call 1
  calling func  : func anonymous
  stack         : [  ]
  running instr : f32.const 40.400_001_525_878_906
  stack         : [ f32.const 40.400_001_525_878_906 ]
  running instr : call 0
  calling func  : func anonymous
  stack         : [  ]
  running instr : f32.const 2.019_999_980_926_513_7
  stack         : [ f32.const 2.019_999_980_926_513_7 ]
  running instr : local.tee 1
  stack         : [ f32.const 2.019_999_980_926_513_7 ]
  running instr : local.get 0
  stack         : [ f32.const 40.400_001_525_878_906 ; f32.const 2.019_999_980_926_513_7 ]
  running instr : f32.add
  stack         : [ f32.const 42.420_001_983_642_578 ]
  stack         : [ f32.const 42.420_001_983_642_578 ]
  running instr : drop
  stack         : [  ]
  stack         : [  ]

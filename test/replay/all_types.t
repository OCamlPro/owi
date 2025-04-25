  $ owi sym -w1 all_types.wat > all_types.scfg
  owi: [ERROR] Trap: unreachable
  owi: [ERROR] Reached problem!
  [13]
  $ owi replay --replay-file all_types.scfg all_types.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 4
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ i32.const 42 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [ i32.const 42 ; i32.const 42 ]
  owi: [INFO] running instr : i32.eq
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i64.const 84
  owi: [INFO] stack         : [ i64.const 84 ]
  owi: [INFO] running instr : call 2
  owi: [INFO] stack         : [ i64.const 84 ; i64.const 84 ]
  owi: [INFO] running instr : i64.eq
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : f32.const 13.119_999_885_559_082
  owi: [INFO] stack         : [ f32.const 13.119_999_885_559_082 ]
  owi: [INFO] running instr : call 1
  owi: [INFO] stack         : [ f32.const 13.119_999_885_559_082 ; f32.const 13.119_999_885_559_082 ]
  owi: [INFO] running instr : f32.eq
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 3
  owi: [INFO] stack         : [ f64.const 12.130_000_000_000_001 ]
  owi: [INFO] running instr : f64.const 12.130_000_000_000_001
  owi: [INFO] stack         : [ f64.const 12.130_000_000_000_001 ; f64.const 12.130_000_000_000_001 ]
  owi: [INFO] running instr : f64.eq
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] running instr : if
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : unreachable
  owi: [ERROR] unreachable
  [94]

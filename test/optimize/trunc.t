**.trunc_** **.trunc_sat_** instructions:
  $ dune exec -- owi --debug --optimize trunc.wast
  parsing      ...
  simplifying  ...
  typechecking ...
  optimizing   ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 2
  calling func : func start
  stack        : [  ]
  running instr: call 0
  calling func : func trunc
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  running instr: call 1
  calling func : func trunc_sat
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]

select instruction:
  $ dune exec -- owi --debug --optimize select.wast
  parsing      ...
  simplifying  ...
  typechecking ...
  optimizing   ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 0
  calling func : func start
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 24
  stack        : [ i32.const 24 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 24
  stack        : [ i64.const 24 ]
  running instr: drop
  stack        : [  ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f32.const 24
  stack        : [ f32.const 24 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 24
  stack        : [ f64.const 24 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

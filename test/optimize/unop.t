unop () instructions:
  $ dune exec -- owi --debug --optimize unop.wast
  parsing      ...
  simplifying  ...
  typechecking ...
  optimizing   ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 3
  calling func : func start
  stack        : [  ]
  running instr: call 0
  calling func : func iunop
  stack        : [  ]
  running instr: i32.const 30
  stack        : [ i32.const 30 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 2
  stack        : [ i32.const 2 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 62
  stack        : [ i64.const 62 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 1
  stack        : [ i64.const 1 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 2
  stack        : [ i64.const 2 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  running instr: call 1
  calling func : func f32unop
  stack        : [  ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  running instr: call 2
  calling func : func f64unop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]

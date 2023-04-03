f32 / f64 binary operations:
  $ dune exec -- owi --debug --optimize fbinop.wast
  simplifying  ...
  typechecking ...
  optimizing   ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 0
  calling func : func start
  stack        : [  ]
  running instr: f32.const 0
  stack        : [ f32.const 0 ]
  running instr: f32.const 42
  stack        : [ f32.const 42 ; f32.const 0 ]
  running instr: f32.add
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 1
  stack        : [ f64.const 1 ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ; f64.const 1 ]
  running instr: f64.mul
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

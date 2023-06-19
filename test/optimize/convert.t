f**.convert_i** instructions:
  $ dune exec -- owi --debug --optimize convert.wast
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  optimizing   ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 0
  calling func : func start
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

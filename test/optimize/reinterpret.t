f**.reinterpret_i** i**.reinterpret_f** instructions:
  $ dune exec -- owi run --debug --optimize reinterpret.wast
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
  running instr: f32.const 0
  stack        : [ f32.const 0 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 0
  stack        : [ f64.const 0 ]
  running instr: drop
  stack        : [  ]
  running instr: i32.const 0
  stack        : [ i32.const 0 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 0
  stack        : [ i64.const 0 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

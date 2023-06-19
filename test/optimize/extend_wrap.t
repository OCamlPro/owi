i**.extend_** i32.wrap_i64 instructions:
  $ dune exec -- owi --debug --optimize extend_wrap.wast
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
  running instr: call 2
  calling func : func start
  stack        : [  ]
  running instr: call 0
  calling func : func extend
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
  running instr: i64.const 42
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  running instr: call 1
  calling func : func wrap
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]

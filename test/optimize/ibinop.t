i32 / i64 binary operations:
  $ dune exec -- owi --debug --optimize ibinop.wast
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
  running instr: i32.const 0
  stack        : [ i32.const 0 ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ; i32.const 0 ]
  running instr: i32.add
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: i64.const 1
  stack        : [ i64.const 1 ]
  running instr: i64.const 42
  stack        : [ i64.const 42 ; i64.const 1 ]
  running instr: i64.mul
  stack        : [ i64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

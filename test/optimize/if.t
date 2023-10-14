if then else instruction:
  $ dune exec -- owi run --debug --optimize if.wast
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
  running instr: (block   (result i32)
    i32.const 42)
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: (block   (result i32)
    i32.const 24)
  stack        : [  ]
  running instr: i32.const 24
  stack        : [ i32.const 24 ]
  stack        : [ i32.const 24 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

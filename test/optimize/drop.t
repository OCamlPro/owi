drop instruction:
  $ dune exec -- owi --debug --optimize drop.wast
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
  running instr: call 1
  calling func : func const
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  running instr: call 2
  calling func : func local_global
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]

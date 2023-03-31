Testing addition on i32:
  $ dune exec -- ../../src/bin/owi.exe --debug --optimize i32.wast
  simplifying  ...
  typechecking ...
  optimizing   ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 0
  calling func : func start
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ]
  running instr: i32.const 41
  stack        : [ i32.const 41 ; i32.const 1 ]
  running instr: i32.add
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

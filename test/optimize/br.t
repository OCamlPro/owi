br instructions:
  $ dune exec -- owi --debug --optimize br.wast
  simplifying  ...
  typechecking ...
  optimizing   ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 2
  calling func : func start
  stack        : [  ]
  running instr: call 0
  calling func : func br
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: br 0
  stack        : [  ]
  running instr: call 1
  calling func : func br_if
  stack        : [  ]
  running instr: br 0
  stack        : [  ]
  stack        : [  ]

  $ owi run memarg.wat --debug
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 0
  calling func : func f
  stack        : [  ]
  running instr: i32.const 4
  stack        : [ i32.const 4 ]
  running instr: i32.const 99999999
  stack        : [ i32.const 99999999 ; i32.const 4 ]
  running instr: i32.store align=1
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ]
  running instr: i32.load align=2
  stack        : [ i32.const -16777216 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]
  $ owi wat2wasm memarg.wat
  $ owi run memarg.wasm --debug
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 0
  calling func : func anonymous
  stack        : [  ]
  running instr: i32.const 4
  stack        : [ i32.const 4 ]
  running instr: i32.const 99999999
  stack        : [ i32.const 99999999 ; i32.const 4 ]
  running instr: i32.store align=1
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ]
  running instr: i32.load align=2
  stack        : [ i32.const -16777216 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

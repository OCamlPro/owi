return instructions:
  $ dune exec -- owi --debug --optimize return.wast
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
  running instr: i32.const 0
  stack        : [ i32.const 0 ]
  running instr: i32.const 0
  stack        : [ i32.const 0 ; i32.const 0 ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ; i32.const 0 ; i32.const 0 ]
  running instr: table.init 0 0
  stack        : [  ]
  running instr: elem.drop 0
  stack        : [  ]
  stack        : [  ]
  running instr: call 3
  calling func : func start
  stack        : [  ]
  running instr: call 0
  calling func : func return
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: return
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: call 1
  calling func : func return_call
  stack        : [  ]
  running instr: return_call 0
  calling func : func return
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: return
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: call 2
  calling func : func return_call_indirect
  stack        : [  ]
  running instr: i32.const 0
  stack        : [ i32.const 0 ]
  running instr: return_call_indirect 0  (result i32)
  calling func : func return
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: return
  stack        : [ i32.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]


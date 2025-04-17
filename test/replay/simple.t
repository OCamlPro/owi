  $ owi replay --replay-file simple.scfg simple.wat
  unreachable
  [94]
  $ owi replay --replay-file simple.scfg simple.wat --debug
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack         : [  ]
  running instr : call 1
  calling func  : func start
  stack         : [  ]
  running instr : i32.const 42
  stack         : [ i32.const 42 ]
  running instr : call 0
  stack         : [ i32.const 42 ; i32.const 42 ]
  running instr : i32.eq
  stack         : [ i32.const 1 ]
  running instr : (if
    (then
      unreachable
    )
  )
  stack         : [  ]
  running instr : unreachable
  unreachable
  [94]

  $ owi sym --invoke-with-symbols --entry-point=f -w1 --print-pc ./print_pc.wat --debug
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
  running instr : call 5
  path condition: [  ]
  calling func  : func anonymous
  stack         : [  ]
  running instr : call 0
  path condition: [  ]
  stack         : [ symbol_0 ]
  running instr : call 1
  path condition: [  ]
  calling func  : func f
  stack         : [  ]
  running instr : local.get 0
  path condition: [  ]
  stack         : [ symbol_0 ]
  running instr : i32.const 1
  path condition: [  ]
  stack         : [ 1 ; symbol_0 ]
  running instr : i32.lt_u
  path condition: [  ]
  stack         : [ (i32.of_bool (i32.lt_u symbol_0 1)) ]
  running instr : if
  path condition: [  ]
  stack         : [  ]
  running instr : unreachable
  path condition: [ (i32.lt_u symbol_0 1) ]
  stack         : [  ]
  stack         : [  ]
  stack         : [  ]
  stack         : [  ]
  Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  Reached problem!
  [13]

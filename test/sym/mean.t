symbolic extern module (assume and assert test):
  $ owi sym mean.wat --debug -w1 --no-value
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
  running instr : call 2
  calling func  : func start
  stack         : [  ]
  running instr : call 0
  stack         : [ symbol_0 ]
  running instr : call 0
  stack         : [ symbol_1 ; symbol_0 ]
  running instr : call 1
  calling func  : func mean
  stack         : [  ]
  running instr : local.get 0
  stack         : [ symbol_0 ]
  running instr : local.get 1
  stack         : [ symbol_1 ; symbol_0 ]
  running instr : i32.add
  stack         : [ (i32.add symbol_0 symbol_1) ]
  running instr : i32.const 2
  stack         : [ 2 ; (i32.add symbol_0 symbol_1) ]
  running instr : i32.div_u
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : local.set 2
  stack         : [  ]
  running instr : local.get 2
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : local.get 0
  stack         : [ symbol_0 ; (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : i32.lt_u
  stack         : [ (i32.of_bool
                     (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2)
                      symbol_0)) ]
  running instr : if
  stack         : [  ]
  running instr : local.get 2
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : local.get 1
  stack         : [ symbol_1 ; (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : i32.lt_u
  stack         : [ (i32.of_bool
                     (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2)
                      symbol_1)) ]
  running instr : if
  stack         : [  ]
  stack         : [  ]
  running instr : local.get 2
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : drop
  stack         : [  ]
  stack         : [  ]
  stack         : [  ]
  running instr : unreachable
  Trap: unreachable
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  Reached problem!
  [13]

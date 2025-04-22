symbolic extern module (assume and assert test):
  $ owi sym mean.wat --debug --print-pc -w1 --no-value
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
  path condition: 
  calling func  : func start
  stack         : [  ]
  running instr : call 0
  path condition: 
  stack         : [ symbol_0 ]
  running instr : call 0
  path condition: 
  stack         : [ symbol_1 ; symbol_0 ]
  running instr : call 1
  path condition: 
  calling func  : func mean
  stack         : [  ]
  running instr : local.get 0
  path condition: 
  stack         : [ symbol_0 ]
  running instr : local.get 1
  path condition: 
  stack         : [ symbol_1 ; symbol_0 ]
  running instr : i32.add
  path condition: 
  stack         : [ (i32.add symbol_0 symbol_1) ]
  running instr : i32.const 2
  path condition: 
  stack         : [ 2 ; (i32.add symbol_0 symbol_1) ]
  running instr : i32.div_u
  path condition: 
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : local.set 2
  path condition: 
  stack         : [  ]
  running instr : local.get 2
  path condition: 
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : local.get 0
  path condition: 
  stack         : [ symbol_0 ; (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : i32.lt_u
  path condition: 
  stack         : [ (i32.of_bool
                     (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2)
                      symbol_0)) ]
  running instr : if
  path condition: 
  stack         : [  ]
  running instr : local.get 2
  path condition: (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2) symbol_0)
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : local.get 1
  path condition: (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2) symbol_0)
  stack         : [ symbol_1 ; (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : i32.lt_u
  path condition: (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2) symbol_0)
  stack         : [ (i32.of_bool
                     (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2)
                      symbol_1)) ]
  running instr : if
  path condition: (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2) symbol_0)
  stack         : [  ]
  stack         : [  ]
  running instr : local.get 2
  path condition: (bool.not
                   (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2)
                    symbol_0))
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  stack         : [ (i32.div_u (i32.add symbol_0 symbol_1) 2) ]
  running instr : drop
  path condition: (bool.not
                   (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2)
                    symbol_0))
  stack         : [  ]
  stack         : [  ]
  stack         : [  ]
  running instr : unreachable
  path condition: (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2) symbol_1),
                  (i32.lt_u (i32.div_u (i32.add symbol_0 symbol_1) 2) symbol_0)
  Trap: unreachable
  model {
    symbol symbol_0 i32
    symbol symbol_1 i32
  }
  Reached problem!
  [13]

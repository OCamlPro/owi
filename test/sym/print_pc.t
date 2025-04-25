  $ owi sym --invoke-with-symbols --entry-point=f -w1 ./print_pc.wat -vv 2>&1 | grep -v "Completed"
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 5
  owi: [DEBUG] path condition: [  ]
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0
  owi: [DEBUG] path condition: [  ]
  owi: [INFO] stack         : [ symbol_0 ]
  owi: [INFO] running instr : call 1
  owi: [DEBUG] path condition: [  ]
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] path condition: [  ]
  owi: [INFO] stack         : [ symbol_0 ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] path condition: [  ]
  owi: [INFO] stack         : [ 1 ; symbol_0 ]
  owi: [INFO] running instr : i32.lt_u
  owi: [DEBUG] path condition: [  ]
  owi: [INFO] stack         : [ (i32.of_bool (i32.lt_u symbol_0 1)) ]
  owi: [INFO] running instr : if
  owi: [DEBUG] path condition: [  ]
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : unreachable
  owi: [DEBUG] path condition: [ (i32.lt_u symbol_0 1) ]
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!

  $ owi wasm abs loop.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : loop (param i32) (result i32)
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ; i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32 {2} ; i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ; i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32 {2} ; i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ; i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : br_if 0


  $ owi wasm abs loop2.wat -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 18
  owi: [INFO] calling func  : func start
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : loop (param i32) (result i32)
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {2} ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32 {2} ; i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {4} ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32 {2} ; i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : br_if 0

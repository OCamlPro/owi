  $ owi abs loop.wat -vv 
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 (func (param i32) (result i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start (local $res i32)
                   i32.const 0
                   (loop (param i32) (result i32)
                     local.get $res
                     i32.const 2
                     i32.add
                     local.set $res
                     local.get $res
                     i32.const 100
                     i32.le_s
                     br_if 0)
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func)
               (func (param i32) (result i32))
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("start", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: loop (param i32) (result i32)
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 2
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: local.set 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 100
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.le_s
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: br_if 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] call (start): abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [DEBUG] jt after (i32.const 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : loop (param i32) (result i32)
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt after (i32.const 2) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt after (i32.add) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt after (local.set 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt after (i32.const 100) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {100} ; i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {100} ; i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt after (i32.le_s) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt after (br_if 0) :  0 -> 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {0}) 
                second : (0 -> i32 {2})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:16)> -> {2}
                                        }
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:16)> -> {2}
                                        }
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt after (i32.const 2) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:16)> -> {2}
                                        }
                 stack  : i32 {2} ; i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt after (i32.add) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:16)> -> {2}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt after (local.set 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:16)> -> {2}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:16)> -> {2}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt after (i32.const 100) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:16)> -> {2}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {100} ; i32 {4} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {100} ; i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt after (i32.le_s) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:16)> -> {2}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {1} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt after (br_if 0) :  0 -> 
                 context: Context{id=2, <(B:16)> -> {2}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {0}
                 locals : i32 {4}
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {2}) 
                second : (0 -> i32 {4})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:16)> -> {4}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:16)> -> {4}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt after (i32.const 2) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:16)> -> {4}
                                        <(B:17)> -> {4}
                                        }
                 stack  : i32 {2} ; i32 {4} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {2} ; i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt after (i32.add) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:16)> -> {4}
                                        <(B:17)> -> BottomMod
                                        }
                 stack  : i32 BottomMod ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt after (local.set 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:16)> -> {4}
                                        <(B:17)> -> BottomMod
                                        }
                 stack  : i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:16)> -> {4}
                                        <(B:17)> -> BottomMod
                                        }
                 stack  : i32 BottomMod ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt after (i32.const 100) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:16)> -> {4}
                                        <(B:17)> -> BottomMod
                                        }
                 stack  : i32 {100} ; i32 BottomMod ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 {100} ; i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt after (i32.le_s) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:20)> -> {};
                                  <(B:16)> -> {4}
                                  <(B:17)> -> BottomMod
                                  }
                 stack  : i32 BottomMod ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt after (br_if 0) :  
  owi: [DEBUG] jt after (loop (param i32) (result i32)) :  
  owi: [DEBUG] abstract state : None 
  
  owi: [DEBUG] jt after (call 0) :  

  $ owi abs loop2.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 (func (param i32) (result i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start (local $res i32)
                   i32.const 0
                   (loop (param i32) (result i32)
                     local.get $res
                     i32.const 2
                     i32.add
                     local.set $res
                     i32.const 100
                     local.get $res
                     i32.le_s
                     br_if 0)
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func)
               (func (param i32) (result i32))
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("start", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: loop (param i32) (result i32)
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 2
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: local.set 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 100
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.le_s
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: br_if 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] call (start): abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [DEBUG] jt after (i32.const 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : loop (param i32) (result i32)
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt after (i32.const 2) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt after (i32.add) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt after (local.set 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt after (i32.const 100) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {100} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {100} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt after (i32.le_s) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt after (br_if 0) :  
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {0}) 
                second : (0 -> i32 {2})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        }
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        }
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt after (i32.const 2) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        }
                 stack  : i32 {2} ; i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt after (i32.add) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt after (local.set 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt after (i32.const 100) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {100} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {100} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {4} ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt after (i32.le_s) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt after (br_if 0) :  
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {2}) 
                second : (0 -> i32 {4})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt after (i32.const 2) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {2} ; i32 {4} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {2} ; i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt after (i32.add) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> BottomMod
                                        }
                 stack  : i32 BottomMod ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt after (local.set 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> BottomMod
                                        }
                 stack  : i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt after (i32.const 100) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> BottomMod
                                        }
                 stack  : i32 {100} ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt after (local.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> BottomMod
                                        }
                 stack  : i32 BottomMod ; i32 {100} ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt after (i32.le_s) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:18)> -> {};
                                  <(B:14)> -> {4}
                                  <(B:15)> -> BottomMod
                                  }
                 stack  : i32 BottomMod ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt after (br_if 0) :  
  owi: [DEBUG] jt after (loop (param i32) (result i32)) :  
  owi: [DEBUG] abstract state : None 
  
  owi: [DEBUG] jt after (call 0) :  

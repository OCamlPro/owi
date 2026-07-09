  $ owi abs assume.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func (result i32))
                 (func (param i32))
                 (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Imported ({
                   modul: "owi"
                   name: "i32_symbol"
                   assigned_name:  $i32_symbol
                   typ:  (result i32)})
                 Imported ({
                   modul: "owi"
                   name: "assume"
                   assigned_name:  $assume
                   typ:  (param i32)})
                 Local ((func $start (local $n i32)
                   call $i32_symbol
                   local.tee $n
                   i32.const 0
                   i32.gt_s
                   call $assume
                   i32.const 100
                   local.get $n
                   i32.div_s
                   i32.const 0
                   i32.gt_s
                   call $assume
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func (result i32))(func (param i32))
               (func)
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("start", 2) ; ("assume", 1) ; ("i32_symbol", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: call 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.tee 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.gt_s
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: call 1
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 100
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.div_s
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.gt_s
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: call 1
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 2
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
  owi: [INFO] running instr : call 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        }
                 stack  : i32 [--..--]
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.tee 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        }
                 stack  : i32 [--..--]
                 locals : i32 [--..--]
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.const 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        }
                 stack  : i32 {0} ; i32 [--..--]
                 locals : i32 [--..--]
  owi: [INFO] stack         : [ i32 {0} ; i32 [--..--] ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : i32.gt_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1,
                                  <(b:10)> -> {true;false};
                                  <(b:11)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:12)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 [--..--]
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [--..--]) ]
  owi: [INFO] running instr : call 1
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:10)> -> {false}; <(b:11)> -> {true};
                                  <(B:9)> -> [1..0x7FFFFFFF]
                                  <(B:12)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:10)> -> {false}; <(b:11)> -> {true};
                                  <(B:9)> -> [1..0x7FFFFFFF]
                                  <(B:12)> -> {0; 1}
                                  }
                 stack  : i32 {100}
                 locals : i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:10)> -> {false}; <(b:11)> -> {true};
                                  <(B:9)> -> [1..0x7FFFFFFF]
                                  <(B:12)> -> {0; 1}
                                  }
                 stack  : i32 [1..0x7FFFFFFF] ; i32 {100}
                 locals : i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [1..0x7FFFFFFF] ; i32 {100} ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.div_s
  owi: [DEBUG] marked 7 as IMPOSSIBLE divide by zero
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:10)> -> {false}; <(b:11)> -> {true};
                                  <(b:15)> -> {false};
                                  <(B:9)> -> [1..0x7FFFFFFF]
                                  <(B:12)> -> {0; 1}
                                  <(B:16)> -> [0..100]
                                  }
                 stack  : i32 [0..100]
                 locals : i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 [0..100] ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.const 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:10)> -> {false}; <(b:11)> -> {true};
                                  <(b:15)> -> {false};
                                  <(B:9)> -> [1..0x7FFFFFFF]
                                  <(B:12)> -> {0; 1}
                                  <(B:16)> -> [0..100]
                                  }
                 stack  : i32 {0} ; i32 [0..100]
                 locals : i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {0} ; i32 [0..100] ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : i32.gt_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:10)> -> {false}; <(b:11)> -> {true};
                                  <(b:15)> -> {false};
                                  <(b:17)> -> {true;false};
                                  <(b:18)> -> {true;false};
                                  <(B:9)> -> [1..0x7FFFFFFF]
                                  <(B:12)> -> {0; 1}
                                  <(B:16)> -> [0..100]
                                  <(B:19)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1}
                 locals : i32 [1..0x7FFFFFFF]
  owi: [INFO] stack         : [ i32 {0; 1} ]
  owi: [INFO] locals        : [ (0 -> i32 [1..0x7FFFFFFF]) ]
  owi: [INFO] running instr : call 1
  [Single_value_abstraction.Noop] Warning: No backpropagation for 'bisdiv'owi: [DEBUG] jt            :  
  owi: [DEBUG] after call(start): abstract state : 
                 context: Context{id=3,
                                  <(b:10)> -> {false}; <(b:11)> -> {true};
                                  <(b:15)> -> {false}; <(b:17)> -> {false};
                                  <(b:18)> -> {true};
                                  <(B:9)> -> [1..0x7FFFFFFF]
                                  <(B:12)> -> {0; 1}
                                  <(B:16)> -> [1..100]
                                  <(B:19)> -> {0; 1}
                                  }
                 stack  : 
                 locals : i32 [1..0x7FFFFFFF]
  
  owi: [DEBUG] jt            :  
  owi: [INFO] Passed division by zero check for expression:(uuid: 7) i32.div_s
  

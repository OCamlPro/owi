  $ owi abs globals.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func (result i32))
                 (func)
                 type_checks: 
                 global: Local ((global $x (mut i32) i32.const 42))
                 table: 
                 mem: 
                 func: Imported ({
                   modul: "owi"
                   name: "i32_symbol"
                   assigned_name:  $i32_symbol
                   typ:  (result i32)})
                 Local ((func $start
                   call $i32_symbol
                   (if
                     (then
                       i32.const 42
                       global.set $x
                     )
                     (else
                       i32.const 54
                       global.set $x
                     )
                   )
                   global.get $x
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func (result i32))
               (func)
               Types names: 
               Global names: ("x", 0)
               Table names: 
               Mem names: 
               Func names: ("start", 1) ; ("i32_symbol", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: call 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: if
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 42
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: global.set 0
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 54
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: global.set 0
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: global.get 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func start
  owi: [DEBUG] call (start): abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [DEBUG] jt after (call 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <(B:9)> -> [--..--]
                                        }
                 stack  : i32 [--..--]
                 locals : 
  owi: [INFO] stack         : [ i32 [--..--] ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : if
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:11)> -> {true}; <(b:12)> -> {true;false};
                                  <(B:9)> -> {0}
                                  }
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:11)> -> {true}; <(b:12)> -> {true;false};
                                  <(B:9)> -> {0}
                                  }
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 54
  owi: [DEBUG] jt after (i32.const 54) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:11)> -> {true}; <(b:12)> -> {true;false};
                                  <(B:9)> -> {0}
                                  }
                 stack  : i32 {54}
                 locals : 
  owi: [INFO] stack         : [ i32 {54} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : global.set 0
  owi: [DEBUG] jt after (global.set 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:11)> -> {false}; <(b:12)> -> {true};
                                  <(B:9)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:11)> -> {false}; <(b:12)> -> {true};
                                  <(B:9)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [DEBUG] jt after (i32.const 42) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2,
                                  <(b:11)> -> {false}; <(b:12)> -> {true};
                                  <(B:9)> -> [1..0xFFFFFFFF]
                                  }
                 stack  : i32 {42}
                 locals : 
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : global.set 0
  owi: [DEBUG] jt after (global.set 0) :  
  owi: [DEBUG] serializing locals (join) : 
                first :  
                second : 
  owi: [DEBUG] serializing stacks (join) : 
                first :  
                second : 
  owi: [DEBUG] jt after (if) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=4,
                                  <(b:11)> -> {true;false};
                                  <(b:12)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:17)> -> {42; 54}
                                  }
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : global.get 0
  owi: [DEBUG] jt after (global.get 0) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=4,
                                  <(b:11)> -> {true;false};
                                  <(b:12)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:17)> -> {42; 54}
                                  }
                 stack  : i32 {42; 54}
                 locals : 
  owi: [INFO] stack         : [ i32 {42; 54} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] jt after (drop) :  
  owi: [DEBUG] after call(start): abstract state : 
                 context: Context{id=4,
                                  <(b:11)> -> {true;false};
                                  <(b:12)> -> {true;false};
                                  <(B:9)> -> [--..--]
                                  <(B:17)> -> {42; 54}
                                  }
                 stack  : 
                 locals : 
  
  owi: [DEBUG] jt after (call 1) :  



  $ owi abs local_func.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func (param $i i32) (result i32))
                 (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $add (param $i i32) (result i32)
                   local.get $i
                   local.get $i
                   i32.add
                   return
                 ))
                 Local ((func $start
                   i32.const 42
                   call $add
                   return
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func (param $i i32) (result i32))
               (func)
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("start", 1) ; ("add", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: return
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 42
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: call 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: return
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
  owi: [INFO] running instr : i32.const 42
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {42}
                 locals : 
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func add
  owi: [DEBUG] call (add): abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {42}
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {42}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {42}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {42}
                 locals : i32 {42}
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 {42}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {42} ; i32 {42}
                 locals : i32 {42}
  owi: [INFO] stack         : [ i32 {42} ; i32 {42} ]
  owi: [INFO] locals        : [ (0 -> i32 {42}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {84}
                 locals : i32 {42}
  owi: [INFO] stack         : [ i32 {84} ]
  owi: [INFO] locals        : [ (0 -> i32 {42}) ]
  owi: [INFO] running instr : return
  owi: [DEBUG] jt            :  ret -> 
                 context: Context{id=1, <empty>}
                 stack  : i32 {84}
                 locals : i32 {42}
  owi: [DEBUG] abstract state : None 
  
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : None 
  
  owi: [DEBUG] jt            :  



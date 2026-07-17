  $ owi abs add.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func (param i32))
                 (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Imported ({
                   modul: "owi"
                   name: "assert"
                   assigned_name:  $assert
                   typ:  (param i32)})
                 Local ((func $start
                   i32.const 42
                   i32.const 28
                   i32.sub
                   i32.const 47
                   i32.add
                   i64.extend_i32_s
                   i64.const 100
                   i64.add
                   i64.const 200
                   i64.gt_s
                   return
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func (param i32))
               (func)
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("start", 1) ; ("assert", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 42
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 28
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.sub
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 47
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i64.extend_i32_s
  owi: [DEBUG] stack             : [i64]
  owi: [DEBUG] typechecking instr: i64.const 100
  owi: [DEBUG] stack             : [i64 i64]
  owi: [DEBUG] typechecking instr: i64.add
  owi: [DEBUG] stack             : [i64]
  owi: [DEBUG] typechecking instr: i64.const 200
  owi: [DEBUG] stack             : [i64 i64]
  owi: [DEBUG] typechecking instr: i64.gt_s
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
  owi: [INFO] running instr : i32.const 28
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {28} ; i32 {42}
                 locals : 
  owi: [INFO] stack         : [ i32 {28} ; i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.sub
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {14}
                 locals : 
  owi: [INFO] stack         : [ i32 {14} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 47
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {47} ; i32 {14}
                 locals : 
  owi: [INFO] stack         : [ i32 {47} ; i32 {14} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {61}
                 locals : 
  owi: [INFO] stack         : [ i32 {61} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.extend_i32_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i64 {61}
                 locals : 
  owi: [INFO] stack         : [ i64 {61} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i64 {100} ; i64 {61}
                 locals : 
  owi: [INFO] stack         : [ i64 {100} ; i64 {61} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i64 {161}
                 locals : 
  owi: [INFO] stack         : [ i64 {161} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.const 200
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i64 {200} ; i64 {161}
                 locals : 
  owi: [INFO] stack         : [ i64 {200} ; i64 {161} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i64.gt_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : 
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : return
  owi: [DEBUG] jt            :  ret -> 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : 
  owi: [DEBUG] abstract state : None 
  
  owi: [DEBUG] jt            :  



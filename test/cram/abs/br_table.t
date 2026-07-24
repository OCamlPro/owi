  $ owi abs br_table.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start
                   (block $b2
                     (block $b1
                       (block $b0
                         i32.const 1
                         br_table $b0 $b1 $b2)
                       i32.const 0
                       return)
                     i32.const 1
                     return)
                   i32.const 2
                   return
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func)
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
  owi: [DEBUG] typechecking instr: block $b2
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: block $b1
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: block $b0
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 1
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: br_table 0 1 2
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: return
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 1
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: return
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 2
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: return
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] before call (start): caller state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block $b2
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block $b1
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block $b0
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt after (i32.const 1) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1}
                 locals : 
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : br_table 0 1 2
  owi: [DEBUG] jt after (br_table 0 1 2) :  1 -> 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [DEBUG] jt after (block $b0) :  0 -> 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [DEBUG] jt after (block $b1) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] jt after (i32.const 1) :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1}
                 locals : 
  owi: [INFO] stack         : [ i32 {1} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : return
  owi: [DEBUG] jt after (return) :  ret -> 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1}
                 locals : 
  owi: [DEBUG] jt after (block $b2) :  ret -> 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1}
                 locals : 
  owi: [DEBUG] after call(start): callee state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  
  owi: [DEBUG] jt after (call 0) :  

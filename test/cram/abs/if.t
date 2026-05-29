  $ owi drun if.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 (func (param i32) (param i32) (result i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start
                   i32.const 42
                   i32.const 28
                   i32.const 0
                   (if (param i32) (param i32) (result i32)
                     (then
                       i32.add
                     )
                     (else
                       i32.sub
                     )
                   )
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func)
               (func (param i32) (param i32) (result i32))
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
  owi: [DEBUG] typechecking instr: i32.const 42
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 28
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: if (param i32) (param i32) (result i32)
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.sub
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : σ:[]; ρ:
  
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ i32.const 42 ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 28
  owi: [INFO] stack         : [ i32.const 28 ; i32.const 42 ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 28 ; i32.const 42 ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : if (param i32) (param i32) (result i32)
  owi: [INFO] stack         : [ i32.const 28 ; i32.const 42 ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block (param i32) (param i32) (result i32)
  owi: [INFO] stack         : [ i32.const 28 ; i32.const 42 ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32.const 14 ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] Func end state : σ:[]; ρ:
  
  
  owi: [INFO] End Abstract_state : σ:[]; ρ:
  
  

  $ owi abs if.wat -vv 
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 (func (param i32) (param i32) (result i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start
                   i32.const 42
                   i32.const 28
                   i32.const 0
                   (if (param i32) (param i32) (result i32)
                     (then
                       i32.add
                     )
                     (else
                       i32.sub
                     )
                   )
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func)
               (func (param i32) (param i32) (result i32))
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
  owi: [DEBUG] typechecking instr: i32.const 42
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 28
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: if (param i32) (param i32) (result i32)
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.sub
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : {
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 42
  owi: [INFO] stack         : [ i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 28
  owi: [INFO] stack         : [ i32 {28} ; i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32 {0} ; i32 {28} ; i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : if (param i32) (param i32) (result i32)
  owi: [INFO] stack         : [ i32 {28} ; i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : block (param i32) (param i32) (result i32)
  owi: [INFO] stack         : [ i32 {28} ; i32 {42} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : i32.sub
  owi: [INFO] stack         : [ i32 {14} ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] func end state : {
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  
  End Abstract_state : {
    ctx : Context{id=1, <empty>}, stack : , locals : 
  }

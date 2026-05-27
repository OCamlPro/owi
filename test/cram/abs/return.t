  $ owi drun return.wat -vv --no-input
  owi: [INFO] parsing      ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start
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
  owi: [INFO] linking      ...
  owi: [DEBUG] σ:[]; ρ:
  
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : σ:[]; ρ:
  
  owi: [DEBUG] σ:[]; ρ:
  
  owi: [DEBUG] σ:[i32.const 2]; ρ:
  
  owi: [DEBUG] Func end state : None
  
  End Abstract_state : none

  $ owi abs return.wat -vv 
  owi: [INFO] parsing      ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start
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
  owi: [INFO] linking      ...
  owi: [DEBUG] #call 0		{
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : {
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  owi: [DEBUG] #i32.const 2		{
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  
  owi: [DEBUG] #return		{
                 ctx : Context{id=1, <empty>}, stack : i32 {2}, locals : 
               }
  
  End Abstract_state : none

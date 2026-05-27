  $ owi drun local_func.wat -vv --no-input
  owi: [INFO] parsing      ...
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
  owi: [INFO] linking      ...
  owi: [DEBUG] σ:[]; ρ:
  
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : σ:[]; ρ:
  
  owi: [DEBUG] σ:[]; ρ:
  
  owi: [DEBUG] σ:[i32.const 42]; ρ:
  
  owi: [INFO] calling func  : func add
  owi: [DEBUG] Func start state : σ:[i32.const 42]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 42]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 42 ; i32.const 42]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 42 ; i32.const 42 ; i32.const 42];
               ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 84 ; i32.const 42]; ρ:0->i32.const 42 
  
  owi: [DEBUG] Func end state : None
  
  owi: [DEBUG] Func end state : None
  
  End Abstract_state : none

  $ owi abs local_func.wat -vv 
  owi: [INFO] parsing      ...
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
  owi: [INFO] linking      ...
  owi: [DEBUG] #call 1		{
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : {
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  owi: [DEBUG] #i32.const 42		{
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  
  owi: [DEBUG] #call 0		{
                 ctx : Context{id=1, <empty>}, stack : i32 {42}, locals : 
               }
  
  owi: [INFO] calling func  : func add
  owi: [DEBUG] Func start state : {
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {42}
               }
  owi: [DEBUG] #local.get 0		{
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {42}
               }
  
  owi: [DEBUG] #local.get 0		{
                 ctx : Context{id=1, <empty>}, stack : i32 {42},
                 locals : i32 {42}
               }
  
  owi: [DEBUG] #i32.add		{
                 ctx : Context{id=1, <empty>}, stack : i32 {42} ; i32 {42},
                 locals : i32 {42}
               }
  
  owi: [DEBUG] #return		{
                 ctx : Context{id=1, <empty>}, stack : i32 {84},
                 locals : i32 {42}
               }
  
  End Abstract_state : none

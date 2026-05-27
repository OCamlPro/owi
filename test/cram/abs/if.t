  $ owi drun if.wat -vv --no-input
  owi: [INFO] parsing      ...
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
  owi: [INFO] linking      ...
  owi: [DEBUG] σ:[]; ρ:
  
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : σ:[]; ρ:
  
  owi: [DEBUG] σ:[]; ρ:
  
  owi: [DEBUG] σ:[i32.const 42]; ρ:
  
  owi: [DEBUG] σ:[i32.const 28 ; i32.const 42]; ρ:
  
  owi: [DEBUG] σ:[i32.const 0 ; i32.const 28 ; i32.const 42]; ρ:
  
  owi: [DEBUG] σ:[i32.const 28 ; i32.const 42]; ρ:
  
  owi: [DEBUG] σ:[i32.const 28 ; i32.const 42]; ρ:
  
  owi: [DEBUG] σ:[i32.const 14]; ρ:
  
  owi: [DEBUG] Func end state : σ:[]; ρ:
  
  
  End Abstract_state : σ:[]; ρ:
  

  $ owi abs if.wat -vv 
  owi: [INFO] parsing      ...
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
  owi: [INFO] linking      ...
  owi: [DEBUG] #call 0		{
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : {
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  owi: [DEBUG] #i32.const 42		{
                 ctx : Context{id=1, <empty>}, stack : , locals : 
               }
  
  owi: [DEBUG] #i32.const 28		{
                 ctx : Context{id=1, <empty>}, stack : i32 {42}, locals : 
               }
  
  owi: [DEBUG] #i32.const 0		{
                 ctx : Context{id=1, <empty>}, stack : i32 {28} ; i32 {42},
                 locals : 
               }
  
  owi: [DEBUG] #if (param i32) (param i32) (result i32)		{
                 ctx : Context{id=1, <empty>},
                 stack : i32 {0} ; i32 {28} ; i32 {42}, locals : 
               }
  
  owi: [DEBUG] #block (param i32) (param i32) (result i32)		{
                 ctx : Context{id=1, <empty>}, stack : i32 {28} ; i32 {42},
                 locals : 
               }
  
  owi: [DEBUG] #i32.add		{
                 ctx : Context{id=1, <empty>}, stack : i32 {28} ; i32 {42},
                 locals : 
               }
  
  owi: [DEBUG] #drop		{
                 ctx : Context{id=1, <empty>}, stack : i32 {70}, locals : 
               }
  
  End Abstract_state : {
    ctx : Context{id=1, <empty>}, stack : , locals : 
  }

  $ owi drun fact.wat -vv --no-input
  owi: [INFO] parsing      ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start (local $n i32) (local $result i32)
                   i32.const 2
                   local.set $n
                   i32.const 1
                   local.set $result
                   (block $done
                     (loop $continue
                       local.get $n
                       i32.const 1
                       i32.le_s
                       br_if $done
                       local.get $result
                       local.get $n
                       i32.mul
                       local.set $result
                       local.get $n
                       i32.const 1
                       i32.sub
                       local.set $n
                       br $continue))
                   local.get $result
                   drop
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
  owi: [DEBUG] Func start state : σ:[]; ρ:0->i32.const 0 1->i32.const 0 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 0 1->i32.const 0 
  
  owi: [DEBUG] σ:[i32.const 2]; ρ:0->i32.const 0 1->i32.const 0 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 2 1->i32.const 0 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 2 1->i32.const 0 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[i32.const 2]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 2]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[i32.const 0]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 1]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[i32.const 2]; ρ:0->i32.const 2 1->i32.const 1 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 2 1->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 2]; ρ:0->i32.const 2 1->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 2]; ρ:0->i32.const 2 1->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 2 1->i32.const 2 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 1 1->i32.const 2 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 1 1->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 1 1->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 1 1->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 1 1->i32.const 2 
  
  owi: [DEBUG] Func end state : None
  
  End Abstract_state : none

  $ owi abs fact.wat -vv 
  owi: [INFO] parsing      ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start (local $n i32) (local $result i32)
                   i32.const 2
                   local.set $n
                   i32.const 1
                   local.set $result
                   (block $done
                     (loop $continue
                       local.get $n
                       i32.const 1
                       i32.le_s
                       br_if $done
                       local.get $result
                       local.get $n
                       i32.mul
                       local.set $result
                       local.get $n
                       i32.const 1
                       i32.sub
                       local.set $n
                       br $continue))
                   local.get $result
                   drop
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
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {0};
                 i32 {0}
               }
  owi: [DEBUG] #i32.const 2		{
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {0};
                 i32 {0}
               }
  
  owi: [DEBUG] #local.set 0		{
                 ctx : Context{id=1, <empty>}, stack : i32 {2},
                 locals : i32 {0}; i32 {0}
               }
  
  owi: [DEBUG] #i32.const 1		{
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {2};
                 i32 {0}
               }
  
  owi: [DEBUG] #local.set 1		{
                 ctx : Context{id=1, <empty>}, stack : i32 {1},
                 locals : i32 {2}; i32 {0}
               }
  
  owi: [DEBUG] #block $done		{
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {2};
                 i32 {1}
               }
  
  owi: [DEBUG] #loop $continue		{
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {2};
                 i32 {1}
               }
  
  owi: [DEBUG] #local.get 0		{
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {2};
                 i32 {1}
               }
  
  owi: [DEBUG] #i32.const 1		{
                 ctx : Context{id=1, <empty>}, stack : i32 {2},
                 locals : i32 {2}; i32 {1}
               }
  
  owi: [DEBUG] #i32.le_s		{
                 ctx : Context{id=1, <empty>}, stack : i32 {1} ; i32 {2},
                 locals : i32 {2}; i32 {1}
               }
  
  owi: [DEBUG] #br_if 1		{
                 ctx : Context{id=1, <empty>}, stack : i32 {0},
                 locals : i32 {2}; i32 {1}
               }
  
  End Abstract_state : none

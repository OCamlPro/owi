  $ owi drun loop.wat -vv --no-input
  owi: [INFO] parsing      ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 (func (param i32) (result i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start (local $res i32)
                   i32.const 0
                   (loop (param i32) (result i32)
                     local.get $res
                     i32.const 2
                     i32.add
                     local.set $res
                     local.get $res
                     i32.const 100
                     i32.le_s
                     br_if 0)
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func)
               (func (param i32) (result i32))
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
  owi: [DEBUG] Func start state : σ:[]; ρ:0->i32.const 0 
  
  owi: [DEBUG] σ:[]; ρ:0->i32.const 0 
  
  owi: [DEBUG] σ:[i32.const 0]; ρ:0->i32.const 0 
  
  owi: [DEBUG] σ:[i32.const 0]; ρ:0->i32.const 0 
  
  owi: [DEBUG] σ:[i32.const 0 ; i32.const 0]; ρ:0->i32.const 0 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 0 ; i32.const 0]; ρ:0->i32.const 0 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 0]; ρ:0->i32.const 0 
  
  owi: [DEBUG] σ:[i32.const 0]; ρ:0->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 0]; ρ:0->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 2 ; i32.const 0]; ρ:0->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 0]; ρ:0->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 1]; ρ:0->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 2 ; i32.const 1]; ρ:0->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 4 ; i32.const 1]; ρ:0->i32.const 2 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 4 
  
  owi: [DEBUG] σ:[i32.const 4 ; i32.const 1]; ρ:0->i32.const 4 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 4 ; i32.const 1]; ρ:0->i32.const 4 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 4 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 4 
  
  owi: [DEBUG] σ:[i32.const 4 ; i32.const 1]; ρ:0->i32.const 4 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 4 ; i32.const 1]; ρ:0->i32.const 4 
  
  owi: [DEBUG] σ:[i32.const 6 ; i32.const 1]; ρ:0->i32.const 4 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 6 
  
  owi: [DEBUG] σ:[i32.const 6 ; i32.const 1]; ρ:0->i32.const 6 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 6 ; i32.const 1]; ρ:0->i32.const 6 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 6 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 6 
  
  owi: [DEBUG] σ:[i32.const 6 ; i32.const 1]; ρ:0->i32.const 6 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 6 ; i32.const 1]; ρ:0->i32.const 6 
  
  owi: [DEBUG] σ:[i32.const 8 ; i32.const 1]; ρ:0->i32.const 6 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 8 
  
  owi: [DEBUG] σ:[i32.const 8 ; i32.const 1]; ρ:0->i32.const 8 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 8 ; i32.const 1]; ρ:0->i32.const 8 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 8 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 8 
  
  owi: [DEBUG] σ:[i32.const 8 ; i32.const 1]; ρ:0->i32.const 8 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 8 ; i32.const 1]; ρ:0->i32.const 8 
  
  owi: [DEBUG] σ:[i32.const 10 ; i32.const 1]; ρ:0->i32.const 8 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 10 
  
  owi: [DEBUG] σ:[i32.const 10 ; i32.const 1]; ρ:0->i32.const 10 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 10 ; i32.const 1];
               ρ:0->i32.const 10 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 10 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 10 
  
  owi: [DEBUG] σ:[i32.const 10 ; i32.const 1]; ρ:0->i32.const 10 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 10 ; i32.const 1]; ρ:0->i32.const 10 
  
  owi: [DEBUG] σ:[i32.const 12 ; i32.const 1]; ρ:0->i32.const 10 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 12 
  
  owi: [DEBUG] σ:[i32.const 12 ; i32.const 1]; ρ:0->i32.const 12 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 12 ; i32.const 1];
               ρ:0->i32.const 12 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 12 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 12 
  
  owi: [DEBUG] σ:[i32.const 12 ; i32.const 1]; ρ:0->i32.const 12 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 12 ; i32.const 1]; ρ:0->i32.const 12 
  
  owi: [DEBUG] σ:[i32.const 14 ; i32.const 1]; ρ:0->i32.const 12 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 14 
  
  owi: [DEBUG] σ:[i32.const 14 ; i32.const 1]; ρ:0->i32.const 14 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 14 ; i32.const 1];
               ρ:0->i32.const 14 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 14 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 14 
  
  owi: [DEBUG] σ:[i32.const 14 ; i32.const 1]; ρ:0->i32.const 14 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 14 ; i32.const 1]; ρ:0->i32.const 14 
  
  owi: [DEBUG] σ:[i32.const 16 ; i32.const 1]; ρ:0->i32.const 14 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 16 
  
  owi: [DEBUG] σ:[i32.const 16 ; i32.const 1]; ρ:0->i32.const 16 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 16 ; i32.const 1];
               ρ:0->i32.const 16 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 16 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 16 
  
  owi: [DEBUG] σ:[i32.const 16 ; i32.const 1]; ρ:0->i32.const 16 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 16 ; i32.const 1]; ρ:0->i32.const 16 
  
  owi: [DEBUG] σ:[i32.const 18 ; i32.const 1]; ρ:0->i32.const 16 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 18 
  
  owi: [DEBUG] σ:[i32.const 18 ; i32.const 1]; ρ:0->i32.const 18 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 18 ; i32.const 1];
               ρ:0->i32.const 18 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 18 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 18 
  
  owi: [DEBUG] σ:[i32.const 18 ; i32.const 1]; ρ:0->i32.const 18 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 18 ; i32.const 1]; ρ:0->i32.const 18 
  
  owi: [DEBUG] σ:[i32.const 20 ; i32.const 1]; ρ:0->i32.const 18 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 20 
  
  owi: [DEBUG] σ:[i32.const 20 ; i32.const 1]; ρ:0->i32.const 20 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 20 ; i32.const 1];
               ρ:0->i32.const 20 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 20 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 20 
  
  owi: [DEBUG] σ:[i32.const 20 ; i32.const 1]; ρ:0->i32.const 20 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 20 ; i32.const 1]; ρ:0->i32.const 20 
  
  owi: [DEBUG] σ:[i32.const 22 ; i32.const 1]; ρ:0->i32.const 20 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 22 
  
  owi: [DEBUG] σ:[i32.const 22 ; i32.const 1]; ρ:0->i32.const 22 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 22 ; i32.const 1];
               ρ:0->i32.const 22 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 22 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 22 
  
  owi: [DEBUG] σ:[i32.const 22 ; i32.const 1]; ρ:0->i32.const 22 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 22 ; i32.const 1]; ρ:0->i32.const 22 
  
  owi: [DEBUG] σ:[i32.const 24 ; i32.const 1]; ρ:0->i32.const 22 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 24 
  
  owi: [DEBUG] σ:[i32.const 24 ; i32.const 1]; ρ:0->i32.const 24 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 24 ; i32.const 1];
               ρ:0->i32.const 24 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 24 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 24 
  
  owi: [DEBUG] σ:[i32.const 24 ; i32.const 1]; ρ:0->i32.const 24 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 24 ; i32.const 1]; ρ:0->i32.const 24 
  
  owi: [DEBUG] σ:[i32.const 26 ; i32.const 1]; ρ:0->i32.const 24 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 26 
  
  owi: [DEBUG] σ:[i32.const 26 ; i32.const 1]; ρ:0->i32.const 26 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 26 ; i32.const 1];
               ρ:0->i32.const 26 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 26 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 26 
  
  owi: [DEBUG] σ:[i32.const 26 ; i32.const 1]; ρ:0->i32.const 26 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 26 ; i32.const 1]; ρ:0->i32.const 26 
  
  owi: [DEBUG] σ:[i32.const 28 ; i32.const 1]; ρ:0->i32.const 26 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 28 
  
  owi: [DEBUG] σ:[i32.const 28 ; i32.const 1]; ρ:0->i32.const 28 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 28 ; i32.const 1];
               ρ:0->i32.const 28 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 28 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 28 
  
  owi: [DEBUG] σ:[i32.const 28 ; i32.const 1]; ρ:0->i32.const 28 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 28 ; i32.const 1]; ρ:0->i32.const 28 
  
  owi: [DEBUG] σ:[i32.const 30 ; i32.const 1]; ρ:0->i32.const 28 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 30 
  
  owi: [DEBUG] σ:[i32.const 30 ; i32.const 1]; ρ:0->i32.const 30 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 30 ; i32.const 1];
               ρ:0->i32.const 30 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 30 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 30 
  
  owi: [DEBUG] σ:[i32.const 30 ; i32.const 1]; ρ:0->i32.const 30 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 30 ; i32.const 1]; ρ:0->i32.const 30 
  
  owi: [DEBUG] σ:[i32.const 32 ; i32.const 1]; ρ:0->i32.const 30 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 32 
  
  owi: [DEBUG] σ:[i32.const 32 ; i32.const 1]; ρ:0->i32.const 32 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 32 ; i32.const 1];
               ρ:0->i32.const 32 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 32 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 32 
  
  owi: [DEBUG] σ:[i32.const 32 ; i32.const 1]; ρ:0->i32.const 32 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 32 ; i32.const 1]; ρ:0->i32.const 32 
  
  owi: [DEBUG] σ:[i32.const 34 ; i32.const 1]; ρ:0->i32.const 32 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 34 
  
  owi: [DEBUG] σ:[i32.const 34 ; i32.const 1]; ρ:0->i32.const 34 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 34 ; i32.const 1];
               ρ:0->i32.const 34 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 34 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 34 
  
  owi: [DEBUG] σ:[i32.const 34 ; i32.const 1]; ρ:0->i32.const 34 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 34 ; i32.const 1]; ρ:0->i32.const 34 
  
  owi: [DEBUG] σ:[i32.const 36 ; i32.const 1]; ρ:0->i32.const 34 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 36 
  
  owi: [DEBUG] σ:[i32.const 36 ; i32.const 1]; ρ:0->i32.const 36 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 36 ; i32.const 1];
               ρ:0->i32.const 36 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 36 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 36 
  
  owi: [DEBUG] σ:[i32.const 36 ; i32.const 1]; ρ:0->i32.const 36 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 36 ; i32.const 1]; ρ:0->i32.const 36 
  
  owi: [DEBUG] σ:[i32.const 38 ; i32.const 1]; ρ:0->i32.const 36 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 38 
  
  owi: [DEBUG] σ:[i32.const 38 ; i32.const 1]; ρ:0->i32.const 38 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 38 ; i32.const 1];
               ρ:0->i32.const 38 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 38 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 38 
  
  owi: [DEBUG] σ:[i32.const 38 ; i32.const 1]; ρ:0->i32.const 38 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 38 ; i32.const 1]; ρ:0->i32.const 38 
  
  owi: [DEBUG] σ:[i32.const 40 ; i32.const 1]; ρ:0->i32.const 38 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 40 
  
  owi: [DEBUG] σ:[i32.const 40 ; i32.const 1]; ρ:0->i32.const 40 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 40 ; i32.const 1];
               ρ:0->i32.const 40 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 40 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 40 
  
  owi: [DEBUG] σ:[i32.const 40 ; i32.const 1]; ρ:0->i32.const 40 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 40 ; i32.const 1]; ρ:0->i32.const 40 
  
  owi: [DEBUG] σ:[i32.const 42 ; i32.const 1]; ρ:0->i32.const 40 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 42 ; i32.const 1]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 42 ; i32.const 1];
               ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 42 ; i32.const 1]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 42 ; i32.const 1]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 44 ; i32.const 1]; ρ:0->i32.const 42 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 44 
  
  owi: [DEBUG] σ:[i32.const 44 ; i32.const 1]; ρ:0->i32.const 44 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 44 ; i32.const 1];
               ρ:0->i32.const 44 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 44 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 44 
  
  owi: [DEBUG] σ:[i32.const 44 ; i32.const 1]; ρ:0->i32.const 44 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 44 ; i32.const 1]; ρ:0->i32.const 44 
  
  owi: [DEBUG] σ:[i32.const 46 ; i32.const 1]; ρ:0->i32.const 44 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 46 
  
  owi: [DEBUG] σ:[i32.const 46 ; i32.const 1]; ρ:0->i32.const 46 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 46 ; i32.const 1];
               ρ:0->i32.const 46 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 46 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 46 
  
  owi: [DEBUG] σ:[i32.const 46 ; i32.const 1]; ρ:0->i32.const 46 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 46 ; i32.const 1]; ρ:0->i32.const 46 
  
  owi: [DEBUG] σ:[i32.const 48 ; i32.const 1]; ρ:0->i32.const 46 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 48 
  
  owi: [DEBUG] σ:[i32.const 48 ; i32.const 1]; ρ:0->i32.const 48 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 48 ; i32.const 1];
               ρ:0->i32.const 48 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 48 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 48 
  
  owi: [DEBUG] σ:[i32.const 48 ; i32.const 1]; ρ:0->i32.const 48 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 48 ; i32.const 1]; ρ:0->i32.const 48 
  
  owi: [DEBUG] σ:[i32.const 50 ; i32.const 1]; ρ:0->i32.const 48 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 50 
  
  owi: [DEBUG] σ:[i32.const 50 ; i32.const 1]; ρ:0->i32.const 50 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 50 ; i32.const 1];
               ρ:0->i32.const 50 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 50 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 50 
  
  owi: [DEBUG] σ:[i32.const 50 ; i32.const 1]; ρ:0->i32.const 50 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 50 ; i32.const 1]; ρ:0->i32.const 50 
  
  owi: [DEBUG] σ:[i32.const 52 ; i32.const 1]; ρ:0->i32.const 50 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 52 
  
  owi: [DEBUG] σ:[i32.const 52 ; i32.const 1]; ρ:0->i32.const 52 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 52 ; i32.const 1];
               ρ:0->i32.const 52 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 52 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 52 
  
  owi: [DEBUG] σ:[i32.const 52 ; i32.const 1]; ρ:0->i32.const 52 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 52 ; i32.const 1]; ρ:0->i32.const 52 
  
  owi: [DEBUG] σ:[i32.const 54 ; i32.const 1]; ρ:0->i32.const 52 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 54 
  
  owi: [DEBUG] σ:[i32.const 54 ; i32.const 1]; ρ:0->i32.const 54 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 54 ; i32.const 1];
               ρ:0->i32.const 54 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 54 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 54 
  
  owi: [DEBUG] σ:[i32.const 54 ; i32.const 1]; ρ:0->i32.const 54 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 54 ; i32.const 1]; ρ:0->i32.const 54 
  
  owi: [DEBUG] σ:[i32.const 56 ; i32.const 1]; ρ:0->i32.const 54 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 56 
  
  owi: [DEBUG] σ:[i32.const 56 ; i32.const 1]; ρ:0->i32.const 56 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 56 ; i32.const 1];
               ρ:0->i32.const 56 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 56 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 56 
  
  owi: [DEBUG] σ:[i32.const 56 ; i32.const 1]; ρ:0->i32.const 56 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 56 ; i32.const 1]; ρ:0->i32.const 56 
  
  owi: [DEBUG] σ:[i32.const 58 ; i32.const 1]; ρ:0->i32.const 56 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 58 
  
  owi: [DEBUG] σ:[i32.const 58 ; i32.const 1]; ρ:0->i32.const 58 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 58 ; i32.const 1];
               ρ:0->i32.const 58 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 58 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 58 
  
  owi: [DEBUG] σ:[i32.const 58 ; i32.const 1]; ρ:0->i32.const 58 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 58 ; i32.const 1]; ρ:0->i32.const 58 
  
  owi: [DEBUG] σ:[i32.const 60 ; i32.const 1]; ρ:0->i32.const 58 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 60 
  
  owi: [DEBUG] σ:[i32.const 60 ; i32.const 1]; ρ:0->i32.const 60 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 60 ; i32.const 1];
               ρ:0->i32.const 60 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 60 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 60 
  
  owi: [DEBUG] σ:[i32.const 60 ; i32.const 1]; ρ:0->i32.const 60 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 60 ; i32.const 1]; ρ:0->i32.const 60 
  
  owi: [DEBUG] σ:[i32.const 62 ; i32.const 1]; ρ:0->i32.const 60 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 62 
  
  owi: [DEBUG] σ:[i32.const 62 ; i32.const 1]; ρ:0->i32.const 62 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 62 ; i32.const 1];
               ρ:0->i32.const 62 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 62 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 62 
  
  owi: [DEBUG] σ:[i32.const 62 ; i32.const 1]; ρ:0->i32.const 62 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 62 ; i32.const 1]; ρ:0->i32.const 62 
  
  owi: [DEBUG] σ:[i32.const 64 ; i32.const 1]; ρ:0->i32.const 62 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 64 
  
  owi: [DEBUG] σ:[i32.const 64 ; i32.const 1]; ρ:0->i32.const 64 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 64 ; i32.const 1];
               ρ:0->i32.const 64 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 64 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 64 
  
  owi: [DEBUG] σ:[i32.const 64 ; i32.const 1]; ρ:0->i32.const 64 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 64 ; i32.const 1]; ρ:0->i32.const 64 
  
  owi: [DEBUG] σ:[i32.const 66 ; i32.const 1]; ρ:0->i32.const 64 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 66 
  
  owi: [DEBUG] σ:[i32.const 66 ; i32.const 1]; ρ:0->i32.const 66 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 66 ; i32.const 1];
               ρ:0->i32.const 66 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 66 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 66 
  
  owi: [DEBUG] σ:[i32.const 66 ; i32.const 1]; ρ:0->i32.const 66 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 66 ; i32.const 1]; ρ:0->i32.const 66 
  
  owi: [DEBUG] σ:[i32.const 68 ; i32.const 1]; ρ:0->i32.const 66 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 68 
  
  owi: [DEBUG] σ:[i32.const 68 ; i32.const 1]; ρ:0->i32.const 68 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 68 ; i32.const 1];
               ρ:0->i32.const 68 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 68 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 68 
  
  owi: [DEBUG] σ:[i32.const 68 ; i32.const 1]; ρ:0->i32.const 68 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 68 ; i32.const 1]; ρ:0->i32.const 68 
  
  owi: [DEBUG] σ:[i32.const 70 ; i32.const 1]; ρ:0->i32.const 68 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 70 
  
  owi: [DEBUG] σ:[i32.const 70 ; i32.const 1]; ρ:0->i32.const 70 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 70 ; i32.const 1];
               ρ:0->i32.const 70 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 70 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 70 
  
  owi: [DEBUG] σ:[i32.const 70 ; i32.const 1]; ρ:0->i32.const 70 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 70 ; i32.const 1]; ρ:0->i32.const 70 
  
  owi: [DEBUG] σ:[i32.const 72 ; i32.const 1]; ρ:0->i32.const 70 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 72 
  
  owi: [DEBUG] σ:[i32.const 72 ; i32.const 1]; ρ:0->i32.const 72 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 72 ; i32.const 1];
               ρ:0->i32.const 72 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 72 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 72 
  
  owi: [DEBUG] σ:[i32.const 72 ; i32.const 1]; ρ:0->i32.const 72 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 72 ; i32.const 1]; ρ:0->i32.const 72 
  
  owi: [DEBUG] σ:[i32.const 74 ; i32.const 1]; ρ:0->i32.const 72 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 74 
  
  owi: [DEBUG] σ:[i32.const 74 ; i32.const 1]; ρ:0->i32.const 74 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 74 ; i32.const 1];
               ρ:0->i32.const 74 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 74 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 74 
  
  owi: [DEBUG] σ:[i32.const 74 ; i32.const 1]; ρ:0->i32.const 74 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 74 ; i32.const 1]; ρ:0->i32.const 74 
  
  owi: [DEBUG] σ:[i32.const 76 ; i32.const 1]; ρ:0->i32.const 74 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 76 
  
  owi: [DEBUG] σ:[i32.const 76 ; i32.const 1]; ρ:0->i32.const 76 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 76 ; i32.const 1];
               ρ:0->i32.const 76 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 76 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 76 
  
  owi: [DEBUG] σ:[i32.const 76 ; i32.const 1]; ρ:0->i32.const 76 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 76 ; i32.const 1]; ρ:0->i32.const 76 
  
  owi: [DEBUG] σ:[i32.const 78 ; i32.const 1]; ρ:0->i32.const 76 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 78 
  
  owi: [DEBUG] σ:[i32.const 78 ; i32.const 1]; ρ:0->i32.const 78 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 78 ; i32.const 1];
               ρ:0->i32.const 78 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 78 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 78 
  
  owi: [DEBUG] σ:[i32.const 78 ; i32.const 1]; ρ:0->i32.const 78 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 78 ; i32.const 1]; ρ:0->i32.const 78 
  
  owi: [DEBUG] σ:[i32.const 80 ; i32.const 1]; ρ:0->i32.const 78 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 80 
  
  owi: [DEBUG] σ:[i32.const 80 ; i32.const 1]; ρ:0->i32.const 80 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 80 ; i32.const 1];
               ρ:0->i32.const 80 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 80 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 80 
  
  owi: [DEBUG] σ:[i32.const 80 ; i32.const 1]; ρ:0->i32.const 80 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 80 ; i32.const 1]; ρ:0->i32.const 80 
  
  owi: [DEBUG] σ:[i32.const 82 ; i32.const 1]; ρ:0->i32.const 80 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 82 
  
  owi: [DEBUG] σ:[i32.const 82 ; i32.const 1]; ρ:0->i32.const 82 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 82 ; i32.const 1];
               ρ:0->i32.const 82 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 82 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 82 
  
  owi: [DEBUG] σ:[i32.const 82 ; i32.const 1]; ρ:0->i32.const 82 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 82 ; i32.const 1]; ρ:0->i32.const 82 
  
  owi: [DEBUG] σ:[i32.const 84 ; i32.const 1]; ρ:0->i32.const 82 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 84 
  
  owi: [DEBUG] σ:[i32.const 84 ; i32.const 1]; ρ:0->i32.const 84 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 84 ; i32.const 1];
               ρ:0->i32.const 84 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 84 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 84 
  
  owi: [DEBUG] σ:[i32.const 84 ; i32.const 1]; ρ:0->i32.const 84 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 84 ; i32.const 1]; ρ:0->i32.const 84 
  
  owi: [DEBUG] σ:[i32.const 86 ; i32.const 1]; ρ:0->i32.const 84 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 86 
  
  owi: [DEBUG] σ:[i32.const 86 ; i32.const 1]; ρ:0->i32.const 86 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 86 ; i32.const 1];
               ρ:0->i32.const 86 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 86 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 86 
  
  owi: [DEBUG] σ:[i32.const 86 ; i32.const 1]; ρ:0->i32.const 86 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 86 ; i32.const 1]; ρ:0->i32.const 86 
  
  owi: [DEBUG] σ:[i32.const 88 ; i32.const 1]; ρ:0->i32.const 86 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 88 
  
  owi: [DEBUG] σ:[i32.const 88 ; i32.const 1]; ρ:0->i32.const 88 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 88 ; i32.const 1];
               ρ:0->i32.const 88 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 88 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 88 
  
  owi: [DEBUG] σ:[i32.const 88 ; i32.const 1]; ρ:0->i32.const 88 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 88 ; i32.const 1]; ρ:0->i32.const 88 
  
  owi: [DEBUG] σ:[i32.const 90 ; i32.const 1]; ρ:0->i32.const 88 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 90 
  
  owi: [DEBUG] σ:[i32.const 90 ; i32.const 1]; ρ:0->i32.const 90 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 90 ; i32.const 1];
               ρ:0->i32.const 90 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 90 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 90 
  
  owi: [DEBUG] σ:[i32.const 90 ; i32.const 1]; ρ:0->i32.const 90 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 90 ; i32.const 1]; ρ:0->i32.const 90 
  
  owi: [DEBUG] σ:[i32.const 92 ; i32.const 1]; ρ:0->i32.const 90 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 92 
  
  owi: [DEBUG] σ:[i32.const 92 ; i32.const 1]; ρ:0->i32.const 92 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 92 ; i32.const 1];
               ρ:0->i32.const 92 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 92 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 92 
  
  owi: [DEBUG] σ:[i32.const 92 ; i32.const 1]; ρ:0->i32.const 92 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 92 ; i32.const 1]; ρ:0->i32.const 92 
  
  owi: [DEBUG] σ:[i32.const 94 ; i32.const 1]; ρ:0->i32.const 92 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 94 
  
  owi: [DEBUG] σ:[i32.const 94 ; i32.const 1]; ρ:0->i32.const 94 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 94 ; i32.const 1];
               ρ:0->i32.const 94 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 94 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 94 
  
  owi: [DEBUG] σ:[i32.const 94 ; i32.const 1]; ρ:0->i32.const 94 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 94 ; i32.const 1]; ρ:0->i32.const 94 
  
  owi: [DEBUG] σ:[i32.const 96 ; i32.const 1]; ρ:0->i32.const 94 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 96 
  
  owi: [DEBUG] σ:[i32.const 96 ; i32.const 1]; ρ:0->i32.const 96 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 96 ; i32.const 1];
               ρ:0->i32.const 96 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 96 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 96 
  
  owi: [DEBUG] σ:[i32.const 96 ; i32.const 1]; ρ:0->i32.const 96 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 96 ; i32.const 1]; ρ:0->i32.const 96 
  
  owi: [DEBUG] σ:[i32.const 98 ; i32.const 1]; ρ:0->i32.const 96 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 98 
  
  owi: [DEBUG] σ:[i32.const 98 ; i32.const 1]; ρ:0->i32.const 98 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 98 ; i32.const 1];
               ρ:0->i32.const 98 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 98 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 98 
  
  owi: [DEBUG] σ:[i32.const 98 ; i32.const 1]; ρ:0->i32.const 98 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 98 ; i32.const 1]; ρ:0->i32.const 98 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 1]; ρ:0->i32.const 98 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 100 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 1]; ρ:0->i32.const 100 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 100 ; i32.const 1];
               ρ:0->i32.const 100 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 100 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 100 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 1]; ρ:0->i32.const 100 
  
  owi: [DEBUG] σ:[i32.const 2 ; i32.const 100 ; i32.const 1];
               ρ:0->i32.const 100 
  
  owi: [DEBUG] σ:[i32.const 102 ; i32.const 1]; ρ:0->i32.const 100 
  
  owi: [DEBUG] σ:[i32.const 1]; ρ:0->i32.const 102 
  
  owi: [DEBUG] σ:[i32.const 102 ; i32.const 1]; ρ:0->i32.const 102 
  
  owi: [DEBUG] σ:[i32.const 100 ; i32.const 102 ; i32.const 1];
               ρ:0->i32.const 102 
  
  owi: [DEBUG] σ:[i32.const 0 ; i32.const 1]; ρ:0->i32.const 102 
  
  owi: [DEBUG] σ:[i32.const 1 ; i32.const 1]; ρ:0->i32.const 102 
  
  owi: [DEBUG] Func end state : σ:[i32.const 1]; ρ:0->i32.const 102 
  
  
  End Abstract_state : σ:[]; ρ:
  

  $ owi abs loop.wat -vv 
  owi: [INFO] parsing      ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func)
                 (func (param i32) (result i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start (local $res i32)
                   i32.const 0
                   (loop (param i32) (result i32)
                     local.get $res
                     i32.const 2
                     i32.add
                     local.set $res
                     local.get $res
                     i32.const 100
                     i32.le_s
                     br_if 0)
                   drop
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func)
               (func (param i32) (result i32))
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
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {0}
               }
  owi: [DEBUG] #i32.const 0		{
                 ctx : Context{id=1, <empty>}, stack : , locals : i32 {0}
               }
  
  owi: [DEBUG] #loop (param i32) (result i32)		{
                 ctx : Context{id=1, <empty>}, stack : i32 {0},
                 locals : i32 {0}
               }
  
  owi: [DEBUG] #local.get 0		{
                 ctx : Context{id=1, <empty>}, stack : i32 {0},
                 locals : i32 {0}
               }
  
  owi: [DEBUG] #i32.const 2		{
                 ctx : Context{id=1, <empty>}, stack : i32 {0} ; i32 {0},
                 locals : i32 {0}
               }
  
  owi: [DEBUG] #i32.add		{
                 ctx : Context{id=1, <empty>},
                 stack : i32 {2} ; i32 {0} ; i32 {0}, locals : i32 {0}
               }
  
  owi: [DEBUG] #local.set 0		{
                 ctx : Context{id=1, <empty>}, stack : i32 {2} ; i32 {0},
                 locals : i32 {0}
               }
  
  owi: [DEBUG] #local.get 0		{
                 ctx : Context{id=1, <empty>}, stack : i32 {0},
                 locals : i32 {2}
               }
  
  owi: [DEBUG] #i32.const 100		{
                 ctx : Context{id=1, <empty>}, stack : i32 {2} ; i32 {0},
                 locals : i32 {2}
               }
  
  owi: [DEBUG] #i32.le_s		{
                 ctx : Context{id=1, <empty>},
                 stack : i32 {100} ; i32 {2} ; i32 {0}, locals : i32 {2}
               }
  
  owi: [DEBUG] #br_if 0		{
                 ctx : Context{id=1, <empty>}, stack : i32 {1} ; i32 {0},
                 locals : i32 {2}
               }
  
  owi: [DEBUG] #drop		{
                 ctx : Context{id=1, <empty>}, stack : i32 {0} ; i32 {0},
                 locals : i32 {2}
               }
  
  End Abstract_state : {
    ctx : Context{id=1, <empty>}, stack : , locals : 
  }

  $ owi drun loop.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] checking     ...
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
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: loop (param i32) (result i32)
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 2
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: local.set 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 100
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.le_s
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: br_if 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] Func start state : σ:[]; ρ:0->i32.const 0 
  
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ 0->i32.const 0  ]
  owi: [INFO] running instr : i32.const 0
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 0  ]
  owi: [INFO] running instr : loop (param i32) (result i32)
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 0  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 0  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 0 ; i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 0  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 0  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 2  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 2  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 2 ; i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 2  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 0 ]
  owi: [INFO] locals        : [ 0->i32.const 2  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 2  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 2  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 2 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 2  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 4 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 2  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 4  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 4 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 4  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 4 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 4  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 4  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 4  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 4 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 4  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 4 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 4  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 6 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 4  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 6  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 6 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 6  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 6 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 6  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 6  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 6  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 6 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 6  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 6 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 6  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 6  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 8  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 8  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 8 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 8  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 8  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 8  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 8 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 8  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 8 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 8  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 10 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 8  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 10  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 10 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 10  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 10 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 10  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 10  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 10  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 10 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 10  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 10 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 10  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 12 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 10  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 12  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 12 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 12  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 12 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 12  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 12  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 12  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 12 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 12  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 12 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 12  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 14 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 12  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 14  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 14 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 14  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 14 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 14  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 14  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 14  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 14 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 14  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 14 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 14  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 16 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 14  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 16  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 16 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 16  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 16 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 16  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 16  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 16  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 16 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 16  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 16 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 16  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 18 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 16  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 18  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 18 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 18  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 18 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 18  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 18  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 18  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 18 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 18  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 18 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 18  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 20 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 18  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 20  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 20 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 20  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 20 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 20  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 20  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 20  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 20 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 20  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 20 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 20  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 22 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 20  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 22  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 22 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 22  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 22 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 22  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 22  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 22  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 22 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 22  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 22 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 22  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 24 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 22  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 24  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 24 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 24  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 24 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 24  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 24  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 24  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 24 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 24  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 24 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 24  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 26 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 24  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 26  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 26 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 26  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 26 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 26  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 26  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 26  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 26 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 26  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 26 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 26  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 28 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 26  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 28  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 28 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 28  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 28 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 28  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 28  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 28  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 28 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 28  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 28 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 28  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 30 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 28  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 30  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 30 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 30  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 30 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 30  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 30  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 30  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 30 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 30  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 30 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 30  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 32 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 30  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 32  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 32 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 32  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 32 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 32  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 32  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 32  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 32 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 32  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 32 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 32  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 34 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 32  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 34  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 34 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 34  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 34 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 34  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 34  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 34  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 34 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 34  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 34 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 34  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 36 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 34  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 36  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 36 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 36  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 36 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 36  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 36  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 36  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 36 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 36  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 36 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 36  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 38 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 36  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 38  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 38 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 38  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 38 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 38  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 38  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 38  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 38 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 38  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 38 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 38  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 40 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 38  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 40  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 40 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 40  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 40 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 40  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 40  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 40  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 40 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 40  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 40 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 40  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 42 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 40  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 42  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 42 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 42  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 42 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 42  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 42  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 42  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 42 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 42  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 42 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 42  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 44 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 42  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 44  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 44 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 44  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 44 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 44  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 44  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 44  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 44 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 44  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 44 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 44  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 46 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 44  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 46  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 46 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 46  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 46 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 46  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 46  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 46  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 46 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 46  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 46 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 46  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 48 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 46  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 48  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 48 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 48  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 48 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 48  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 48  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 48  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 48 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 48  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 48 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 48  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 50 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 48  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 50  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 50 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 50  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 50 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 50  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 50  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 50  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 50 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 50  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 50 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 50  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 52 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 50  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 52  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 52 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 52  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 52 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 52  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 52  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 52  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 52 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 52  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 52 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 52  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 54 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 52  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 54  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 54 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 54  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 54 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 54  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 54  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 54  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 54 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 54  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 54 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 54  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 56 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 54  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 56  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 56 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 56  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 56 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 56  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 56  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 56  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 56 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 56  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 56 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 56  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 58 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 56  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 58  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 58 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 58  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 58 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 58  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 58  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 58  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 58 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 58  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 58 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 58  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 60 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 58  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 60  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 60 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 60  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 60 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 60  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 60  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 60  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 60 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 60  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 60 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 60  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 62 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 60  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 62  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 62 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 62  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 62 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 62  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 62  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 62  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 62 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 62  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 62 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 62  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 64 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 62  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 64  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 64 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 64  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 64 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 64  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 64  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 64  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 64 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 64  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 64 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 64  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 66 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 64  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 66  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 66 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 66  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 66 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 66  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 66  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 66  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 66 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 66  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 66 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 66  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 68 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 66  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 68  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 68 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 68  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 68 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 68  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 68  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 68  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 68 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 68  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 68 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 68  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 70 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 68  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 70  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 70 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 70  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 70 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 70  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 70  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 70  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 70 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 70  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 70 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 70  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 72 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 70  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 72  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 72 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 72  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 72 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 72  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 72  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 72  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 72 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 72  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 72 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 72  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 74 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 72  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 74  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 74 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 74  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 74 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 74  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 74  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 74  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 74 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 74  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 74 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 74  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 76 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 74  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 76  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 76 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 76  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 76 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 76  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 76  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 76  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 76 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 76  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 76 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 76  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 78 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 76  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 78  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 78 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 78  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 78 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 78  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 78  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 78  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 78 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 78  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 78 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 78  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 80 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 78  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 80  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 80 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 80  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 80 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 80  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 80  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 80  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 80 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 80  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 80 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 80  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 82 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 80  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 82  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 82 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 82  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 82 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 82  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 82  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 82  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 82 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 82  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 82 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 82  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 84 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 82  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 84  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 84 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 84  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 84 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 84  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 84  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 84  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 84 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 84  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 84 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 84  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 86 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 84  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 86  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 86 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 86  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 86 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 86  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 86  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 86  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 86 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 86  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 86 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 86  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 88 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 86  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 88  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 88 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 88  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 88 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 88  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 88  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 88  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 88 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 88  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 88 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 88  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 90 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 88  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 90  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 90 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 90  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 90 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 90  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 90  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 90  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 90 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 90  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 90 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 90  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 92 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 90  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 92  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 92 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 92  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 92 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 92  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 92  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 92  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 92 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 92  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 92 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 92  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 94 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 92  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 94  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 94 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 94  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 94 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 94  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 94  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 94  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 94 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 94  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 94 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 94  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 96 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 94  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 96  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 96 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 96  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 96 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 96  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 96  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 96  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 96 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 96  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 96 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 96  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 98 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 96  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 98  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 98 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 98  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 98 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 98  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 98  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 98  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 98 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 98  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 98 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 98  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 98  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 100  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 100  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 100 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 100  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 100  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 100  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 100  ]
  owi: [INFO] running instr : i32.const 2
  owi: [INFO] stack         : [ i32.const 2 ; i32.const 100 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 100  ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ i32.const 102 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 100  ]
  owi: [INFO] running instr : local.set 0
  owi: [INFO] stack         : [ i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 102  ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ i32.const 102 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 102  ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ i32.const 100 ; i32.const 102 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 102  ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ i32.const 0 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 102  ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ i32.const 1 ; i32.const 1 ]
  owi: [INFO] locals        : [ 0->i32.const 102  ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] Func end state : σ:[i32.const 1]; ρ:0->i32.const 102 
  
  
  owi: [INFO] End Abstract_state : σ:[]; ρ:
  
  

  $ owi abs loop.wat -vv 
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] checking     ...
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
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: loop (param i32) (result i32)
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 2
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: local.set 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 100
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.le_s
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: br_if 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] call (start): abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : loop (param i32) (result i32)
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {100} ; i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {100} ; i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {1} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {0}) 
                second : (0 -> i32 {2})
  owi: [DEBUG] serializing stacks (join) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {0}) 
                second : (0 -> i32 {0; 2})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {0; 2}
                                        }
                 stack  : i32 {0}
                 locals : i32 {0; 2}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {0; 2}
                                        }
                 stack  : i32 {0; 2} ; i32 {0}
                 locals : i32 {0; 2}
  owi: [INFO] stack         : [ i32 {0; 2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 2}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {0; 2}
                                        }
                 stack  : i32 {2} ; i32 {0; 2} ; i32 {0}
                 locals : i32 {0; 2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0; 2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 2}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {0; 2}
                                        <(B:19)> -> {2; 4}
                                        }
                 stack  : i32 {2; 4} ; i32 {0}
                 locals : i32 {0; 2}
  owi: [INFO] stack         : [ i32 {2; 4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0; 2}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {0; 2}
                                        <(B:19)> -> {2; 4}
                                        }
                 stack  : i32 {0}
                 locals : i32 {2; 4}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2; 4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {0; 2}
                                        <(B:19)> -> {2; 4}
                                        }
                 stack  : i32 {2; 4} ; i32 {0}
                 locals : i32 {2; 4}
  owi: [INFO] stack         : [ i32 {2; 4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2; 4}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:18)> -> {0; 2}
                                        <(B:19)> -> {2; 4}
                                        }
                 stack  : i32 {100} ; i32 {2; 4} ; i32 {0}
                 locals : i32 {2; 4}
  owi: [INFO] stack         : [ i32 {100} ; i32 {2; 4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2; 4}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:20)> -> {true};
                                  <(B:18)> -> {0; 2}
                                  <(B:19)> -> {2; 4}
                                  }
                 stack  : i32 {1} ; i32 {0}
                 locals : i32 {2; 4}
  owi: [INFO] stack         : [ i32 {1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2; 4}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=3,
                                  <(b:20)> -> {true};
                                  <(B:18)> -> {0; 2}
                                  <(B:19)> -> {2; 4}
                                  }
                 stack  : i32 {0}
                 locals : i32 {2; 4}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {0}) 
                second : (0 -> i32 {2; 4})
  owi: [DEBUG] serializing stacks (join) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {0; 2}) 
                second : (0 -> i32 {0; 2; 4})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=5, <(B:18)> -> [0..0x7FFFFFFE],0%2
                                        }
                 stack  : i32 {0}
                 locals : i32 [0..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [0..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5, <(B:18)> -> [0..0x7FFFFFFE],0%2
                                        }
                 stack  : i32 [0..0x7FFFFFFE],0%2 ; i32 {0}
                 locals : i32 [0..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 [0..0x7FFFFFFE],0%2 ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [0..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5, <(B:18)> -> [0..0x7FFFFFFE],0%2
                                        }
                 stack  : i32 {2} ; i32 [0..0x7FFFFFFE],0%2 ; i32 {0}
                 locals : i32 [0..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {2} ; i32 [0..0x7FFFFFFE],0%2 ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [0..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:18)> -> [0..0x7FFFFFFE],0%2
                                  <(B:19)> -> [2..0x80000000],0%2
                                  }
                 stack  : i32 [2..0x80000000],0%2 ; i32 {0}
                 locals : i32 [0..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 [2..0x80000000],0%2 ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [0..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:18)> -> [0..0x7FFFFFFE],0%2
                                  <(B:19)> -> [2..0x80000000],0%2
                                  }
                 stack  : i32 {0}
                 locals : i32 [2..0x80000000],0%2
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [2..0x80000000],0%2) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:18)> -> [0..0x7FFFFFFE],0%2
                                  <(B:19)> -> [2..0x80000000],0%2
                                  }
                 stack  : i32 [2..0x80000000],0%2 ; i32 {0}
                 locals : i32 [2..0x80000000],0%2
  owi: [INFO] stack         : [ i32 [2..0x80000000],0%2 ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [2..0x80000000],0%2) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(B:18)> -> [0..0x7FFFFFFE],0%2
                                  <(B:19)> -> [2..0x80000000],0%2
                                  }
                 stack  : i32 {100} ; i32 [2..0x80000000],0%2 ; i32 {0}
                 locals : i32 [2..0x80000000],0%2
  owi: [INFO] stack         : [ i32 {100} ; i32 [2..0x80000000],0%2 ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [2..0x80000000],0%2) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=5,
                                  <(b:20)> -> {true;false};
                                  <(B:18)> -> [0..0x7FFFFFFE],0%2
                                  <(B:19)> -> [2..0x80000000],0%2
                                  <(B:23)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1} ; i32 {0}
                 locals : i32 [2..0x80000000],0%2
  owi: [INFO] stack         : [ i32 {0; 1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [2..0x80000000],0%2) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=6,
                                  <(b:20)> -> {true}; <(b:24)> -> {true;false};
                                  <(B:18)> -> [0..0x7FFFFFFE],0%2
                                  <(B:19)> -> {signed: [-0x80000000..100],0%2; unsigned: [2..0x80000000],0%2}
                                  <(B:23)> -> {0; 1}
                                  }
                 stack  : i32 {0}
                 locals : i32 {signed: [-0x80000000..100],0%2; unsigned: [2..0x80000000],0%2}
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {0}) 
                second : (0 -> i32 {signed: [-0x80000000..100],0%2; unsigned: [2..0x80000000],0%2})
  owi: [DEBUG] serializing stacks (join) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 [0..0x7FFFFFFE],0%2) 
                second : (0 -> i32 {signed: [-0x80000000..100],0%2; unsigned: [0..0x80000000],0%2})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 {0}
                 locals : i32 [-0x80000000..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 {0}
                 locals : i32 [-0x80000000..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 {2} ; i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 
                 {0}
                 locals : i32 [-0x80000000..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {2} ; i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 
              {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  <(B:19)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 {0}
                 locals : i32 [-0x80000000..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  <(B:19)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 {0}
                 locals : i32 [-0x80000000..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  <(B:19)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 {0}
                 locals : i32 [-0x80000000..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  <(B:19)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  }
                 stack  : i32 {100} ; i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 
                 {0}
                 locals : i32 [-0x80000000..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {100} ; i32 [-0x80000000..0x7FFFFFFE],0%2 ; i32 
              {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=9,
                                  <(b:20)> -> {true;false};
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  <(B:19)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  <(B:23)> -> {0; 1}
                                  }
                 stack  : i32 {0; 1} ; i32 {0}
                 locals : i32 [-0x80000000..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {0; 1} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  0 -> 
                 context: Context{id=10,
                                  <(b:20)> -> {true}; <(b:24)> -> {true;false};
                                  <(B:18)> -> [-0x80000000..0x7FFFFFFE],0%2
                                  <(B:19)> -> [-0x80000000..100],0%2
                                  <(B:23)> -> {0; 1}
                                  }
                 stack  : i32 {0}
                 locals : i32 [-0x80000000..100],0%2
  owi: [DEBUG] serializing locals (join) : 
                first : (0 -> i32 {0}) 
                second : (0 -> i32 [-0x80000000..100],0%2)
  owi: [DEBUG] serializing stacks (join) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 [-0x80000000..0x7FFFFFFE],0%2) 
                second : (0 -> i32 [-0x80000000..100],0%2)
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=11,
                                  <(b:20)> -> {false}; <(b:24)> -> {true};
                                  <(B:18)> -> [100..0x7FFFFFFC],0%2
                                  <(B:19)> -> [102..0x7FFFFFFE],0%2
                                  <(B:23)> -> {0; 1}
                                  }
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 [102..0x7FFFFFFE],0%2
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 [102..0x7FFFFFFE],0%2) ]
  owi: [INFO] running instr : drop
  owi: [DEBUG] jt            :  
  owi: [DEBUG] after call(start): abstract state : 
                 context: Context{id=11,
                                  <(b:20)> -> {false}; <(b:24)> -> {true};
                                  <(B:18)> -> [100..0x7FFFFFFC],0%2
                                  <(B:19)> -> [102..0x7FFFFFFE],0%2
                                  <(B:23)> -> {0; 1}
                                  }
                 stack  : i32 {0}
                 locals : i32 [102..0x7FFFFFFE],0%2
  
  owi: [DEBUG] jt            :  

  $ owi abs loop2.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] checking     ...
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
                     i32.const 100
                     local.get $res
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
  owi: [INFO] typechecking ...
  owi: [DEBUG] stack             : []
  owi: [DEBUG] typechecking instr: i32.const 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: loop (param i32) (result i32)
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: i32.const 2
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.add
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: local.set 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: i32.const 100
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: local.get 0
  owi: [DEBUG] stack             : [i32 i32 i32]
  owi: [DEBUG] typechecking instr: i32.le_s
  owi: [DEBUG] stack             : [i32 i32]
  owi: [DEBUG] typechecking instr: br_if 0
  owi: [DEBUG] stack             : [i32]
  owi: [DEBUG] typechecking instr: drop
  owi: [INFO] linking      ...
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : 
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [  ]
  owi: [INFO] running instr : call 0
  owi: [INFO] calling func  : func start
  owi: [DEBUG] call (start): abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : 
                 locals : i32 {0}
  owi: [INFO] stack         : [  ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : loop (param i32) (result i32)
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {0}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {0}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {100} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {2} ; i32 {100} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=1, <empty>}
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {0}) 
                second : (0 -> i32 {2})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        }
                 stack  : i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        }
                 stack  : i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        }
                 stack  : i32 {2} ; i32 {2} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {2} ; i32 {2} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {0}
                 locals : i32 {2}
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {2}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {100} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {100} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {4} ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=2, <(B:14)> -> {2}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {0} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {0} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] serializing locals (widen) : 
                first : (0 -> i32 {2}) 
                second : (0 -> i32 {4})
  owi: [DEBUG] serializing stacks (widen) : 
                first : i32 {0} 
                second : i32 {0}
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {4} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.const 2
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> {4}
                                        }
                 stack  : i32 {2} ; i32 {4} ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 {2} ; i32 {4} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : i32.add
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> BottomMod
                                        }
                 stack  : i32 BottomMod ; i32 {0}
                 locals : i32 {4}
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 {4}) ]
  owi: [INFO] running instr : local.set 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> BottomMod
                                        }
                 stack  : i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.const 100
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> BottomMod
                                        }
                 stack  : i32 {100} ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3, <(B:14)> -> {4}
                                        <(B:15)> -> BottomMod
                                        }
                 stack  : i32 BottomMod ; i32 {100} ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {100} ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : i32.le_s
  owi: [DEBUG] jt            :  
  owi: [DEBUG] abstract state : 
                 context: Context{id=3,
                                  <(b:18)> -> {};
                                  <(B:14)> -> {4}
                                  <(B:15)> -> BottomMod
                                  }
                 stack  : i32 BottomMod ; i32 {0}
                 locals : i32 BottomMod
  owi: [INFO] stack         : [ i32 BottomMod ; i32 {0} ]
  owi: [INFO] locals        : [ (0 -> i32 BottomMod) ]
  owi: [INFO] running instr : br_if 0
  owi: [DEBUG] jt            :  
  owi: internal error, uncaught exception:
       File "src/abstract/abstract_driver.ml", line 351, characters 14-20: Assertion failed
       Raised at Owi__Abstract_driver.DenotFixpoint.eval_instr.(fun).fixpoint in file "src/abstract/abstract_driver.ml", line 351, characters 14-26
       Called from Owi__Abstract_driver.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_driver.ml", line 179, characters 32-54
       Called from Owi__Abstract_driver.DenotFixpoint.eval_func in file "src/abstract/abstract_driver.ml", line 218, characters 28-56
       Called from Owi__Abstract_driver.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_driver.ml", line 257, characters 16-49
       Called from Owi__Abstract_driver.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_driver.ml", line 179, characters 32-54
       Called from Owi__Abstract_driver.expr.(fun) in file "src/abstract/abstract_driver.ml", line 652, characters 14-48
       Called from Stdlib__List.fold_left in file "list.ml", line 125, characters 24-34
       Called from Owi__Abstract_driver.expr in file "src/abstract/abstract_driver.ml", lines 649-655, characters 4-20
       Called from Owi__Cmd_abs.cmd in file "src/cmd/cmd_abs.ml", line 17, characters 4-37
       Called from Stdlib__Result.map in file "result.ml", line 27, characters 32-37
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 41, characters 7-16
  [125]

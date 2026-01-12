  $ owi sym --invoke-with-symbols --entry-point=f -w1 ./print_pc.wat -vv 2>&1 | grep -v "Completed"
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 function_type: (func (result i32))
                 (func (param $x i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Imported ({
                   modul: "owi"
                   name: "i32_symbol"
                   assigned_name:  $i32_symbol
                   typ:  (result i32)})
                 Local ((func $f (param $x i32)
                   local.get $x
                   i32.const 1
                   i32.lt_u
                   (if
                     (then
                       unreachable
                     )
                   )
                 ))
                 elem: 
                 data: 
                 start: 
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func (result i32))
               (func (param $x i32))
               Types names: 
               Global names: 
               Table names: 
               Mem names: 
               Func names: ("f", 1) ; ("i32_symbol", 0)
               Elem names: 
               Data names: 
               Tag names: 
               
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 6 (executed 0 times)
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0 (executed 0 times)
  owi: [INFO] stack         : [ symbol_0 ]
  owi: [INFO] running instr : call 5 (executed 0 times)
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0 (executed 0 times)
  owi: [INFO] stack         : [ symbol_0 ]
  owi: [INFO] running instr : i32.const 1 (executed 0 times)
  owi: [INFO] stack         : [ 1 ; symbol_0 ]
  owi: [INFO] running instr : i32.lt_u (executed 0 times)
  owi: [INFO] stack         : [ (i32.of_bool (i32.lt_u symbol_0 1)) ]
  owi: [INFO] running instr : if (executed 0 times)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : unreachable (executed 0 times)
  owi: [DEBUG] path condition smt query:
                (let-const symbol_0 i32)
                (assert (i32.lt_u symbol_0 (i32 1)))
                (check-sat)
  owi: [ERROR] Trap: unreachable
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!

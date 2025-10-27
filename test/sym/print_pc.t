  $ owi sym --invoke-with-symbols --entry-point=f -w1 ./print_pc.wat -vv 2>&1 | grep -v "Completed"
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: []
                 function_type: [(func (param $x i32)) ; (func (result i32))]
                 type_checks: []
                 global: []
                 table: []
                 mem: []
                 func: [{ index = 1 ; value = Local ((func $f (param $x i32)
                   local.get $x
                   i32.const 1
                   i32.lt_u
                   (if
                     (then
                       unreachable
                     )
                   )
                 )) } ; { index = 0 ; value = Imported ({
                   modul: "owi"
                   name: "i32_symbol"
                   assigned_name:  $i32_symbol
                   desc:  (result i32)}) }]
                 elem: []
                 data: []
                 exports: {
                   global: []
                   mem: []
                   table: []
                   func: [("f", $f)]
                   }
                 start: 
                 annots: []
               }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] {
                 id: 
                 typ: {
                   values: [{ index = 0 ; value = (func (result i32)) } ; { index = 1 ; value = (func (param $x i32)) }]
                   named: }
                 global: {
                   values: []
                   named: }
                 table: {
                   values: []
                   named: }
                 mem: {
                   values: []
                   named: }
                 func: {
                   values: [{ index = 1 ; value = Local ((func $f (param $x i32)
                     local.get $x
                     i32.const 1
                     i32.lt_u
                     (if
                       (then
                         unreachable
                       )
                     )
                   )) } ; { index = 0 ; value = Imported ({
                     modul: "owi"
                     name: "i32_symbol"
                     assigned_name:  $i32_symbol
                     desc:  (result i32)}) }]
                   named: ("f", 1) ; ("i32_symbol", 0)}
                 elem: {
                   values: []
                   named: }
                 data: {
                   values: []
                   named: }
                 exports: {
                   global: []
                   mem: []
                   table: []
                   func: [("f", $f)]
                   }
                 start: 
                 annots: []
               }
  owi: [DEBUG] rewriting    ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 6
  owi: [DEBUG] path condition smt query: empty
  owi: [INFO] calling func  : func anonymous
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 0
  owi: [DEBUG] path condition smt query: empty
  owi: [INFO] stack         : [ symbol_0 ]
  owi: [INFO] running instr : call 5
  owi: [DEBUG] path condition smt query: empty
  owi: [INFO] calling func  : func f
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : local.get 0
  owi: [DEBUG] path condition smt query: empty
  owi: [INFO] stack         : [ symbol_0 ]
  owi: [INFO] running instr : i32.const 1
  owi: [DEBUG] path condition smt query: empty
  owi: [INFO] stack         : [ 1 ; symbol_0 ]
  owi: [INFO] running instr : i32.lt_u
  owi: [DEBUG] path condition smt query: empty
  owi: [INFO] stack         : [ (i32.of_bool (i32.lt_u symbol_0 1)) ]
  owi: [INFO] running instr : if
  owi: [DEBUG] path condition smt query: empty
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : unreachable
  owi: [DEBUG] path condition smt query:
                (let-const symbol_0 i32)
                (assert (i32.lt_u symbol_0 (i32 1)))
                (check-sat)
  owi: [ERROR] Trap: unreachable
  owi: [DEBUG] scope tokens: [symbol symbol_0]
  model {
    symbol symbol_0 i32 0
  }
  owi: [ERROR] Reached problem!

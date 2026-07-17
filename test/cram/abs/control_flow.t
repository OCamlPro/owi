  $ owi abs control_flow.wat -vv
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [DEBUG] grouping     ...
  owi: [DEBUG] {id: 
                 typ: 
                 decl_types: (func (param $mode i32) (param $value i32) (result i32))
                 (func (result i32))
                 type_checks: 
                 global: 
                 table: 
                 mem: 
                 func: Local ((func $start (param $mode i32) (param $value i32) (result i32) (local $result i32)
                   (block $end
                     (block $mode2
                       (block $mode1
                         (block $mode0
                           local.get $mode
                           br_table $mode0 $mode1 $mode2 $end)
                         local.get $value
                         i32.const 0
                         i32.gt_s
                         (if (result i32)
                           (then
                             local.get $value
                             i32.const 10
                             i32.add
                           )
                           (else
                             i32.const 0
                           )
                         )
                         local.set $result
                         br $end)
                       local.get $value
                       local.set $result
                       (loop $countdown
                         local.get $result
                         i32.const 0
                         i32.le_s
                         (if
                           (then
                             br 2
                           )
                         )
                         local.get $result
                         i32.const 1
                         i32.sub
                         local.set $result
                         br $countdown)
                       br $end)
                     local.get $value
                     i32.const 2
                     i32.mul
                     local.set $result)
                   local.get $result
                 ))
                 elem: 
                 data: 
                 start: $start
                 }
  owi: [DEBUG] assigning    ...
  owi: [DEBUG] Types: (func (param $mode i32) (param $value i32) (result i32))
               (func (result i32))
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
  owi: [ERROR] start function must have type [] -> []
  [32]

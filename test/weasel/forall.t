  $ owi instrument forall.wat
  $ cat forall.instrumented.wat
  (module
    (import "owi" "assert" (func $assert  (param i32)))
    (type (func))
    (type (func (param i32)))
    (type (func (result i32)))
    (type (func (param i32) (result i32)))
    (func $start
      
    )
    (func $__weasel_start (local $__weasel_temp i32) (local $__weasel_binder_0 i32)
      (block $__weasel_forall (result i32)
        i32.const 1
        local.set 1
        i32.const 1
        (loop $__weasel_loop (param i32) (result i32)
          local.get 1
          i32.const 100
          i32.le_s
          i32.and
          local.tee 0
          local.get 0
          i32.const 1
          i32.xor
          br_if 1
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 1
          i32.const 10
          i32.le_s
          br_if 0))
      call 0
      call 1
    )
    (start 2)
  )
  $ owi instrument forall.wat --symbolic
  $ cat forall.instrumented.wat
  (module
    (import "owi" "assert" (func $assert  (param i32)))
    (type (func))
    (type (func (param i32)))
    (type (func (result i32)))
    (type (func (param i32) (result i32)))
    (func $start
      
    )
    (func $__weasel_start (local $__weasel_temp i32) (local $__weasel_binder_0 i32)
      (block $__weasel_forall (result i32)
        i32.const 1
        local.set 1
        i32.const 1
        (loop $__weasel_loop (param i32) (result i32)
          local.get 1
          i32.const 100
          i32.le_s
          i32.and
          local.tee 0
          local.get 0
          i32.const 1
          i32.xor
          br_if 1
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          local.get 1
          i32.const 10
          i32.le_s
          br_if 0))
      call 0
      call 1
    )
    (start 2)
  )
  $ owi sym forall.wat --rac -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 2
  owi: [INFO] calling func  : func __weasel_start
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : block $__weasel_forall (result i32)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : loop $__weasel_loop (param i32) (result i32)
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 2 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 2 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 2 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 2 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 2 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 2 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 2 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 3 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 3 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 3 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 3 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 3 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 3 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 3 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 4 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 4 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 4 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 4 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 4 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 4 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 4 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 5 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 5 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 5 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 5 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 5 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 5 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 5 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 6 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 6 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 6 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 6 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 6 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 6 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 6 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 7 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 7 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 7 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 7 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 7 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 7 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 7 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 8 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 8 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 8 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 8 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 8 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 8 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 8 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 9 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 9 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 9 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 9 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 9 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 9 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 9 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 10 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 10 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 10 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 10 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 10 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 10 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 10 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 11 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 11 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 11 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func start
  owi: [INFO] Completed paths: 1
  All OK!
  $ owi sym forall.wat --srac -v
  owi: [INFO] parsing      ...
  owi: [INFO] checking     ...
  owi: [INFO] typechecking ...
  owi: [INFO] typechecking ...
  owi: [INFO] linking      ...
  owi: [INFO] interpreting ...
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 2
  owi: [INFO] calling func  : func __weasel_start
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : block $__weasel_forall (result i32)
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : loop $__weasel_loop (param i32) (result i32)
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 2 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 2 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 2 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 2 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 2 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 2 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 2 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 3 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 3 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 3 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 3 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 3 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 3 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 3 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 4 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 4 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 4 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 4 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 4 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 4 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 4 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 5 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 5 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 5 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 5 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 5 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 5 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 5 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 6 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 6 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 6 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 6 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 6 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 6 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 6 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 7 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 7 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 7 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 7 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 7 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 7 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 7 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 8 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 8 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 8 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 8 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 8 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 8 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 8 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 9 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 9 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 9 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 9 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 9 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 9 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 9 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 10 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 10 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 10 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 10 ; 1 ]
  owi: [INFO] running instr : i32.const 100
  owi: [INFO] stack         : [ 100 ; 10 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.and
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.tee 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 0
  owi: [INFO] stack         : [ 1 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 1 ; 1 ]
  owi: [INFO] running instr : i32.xor
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 10 ; 1 ]
  owi: [INFO] running instr : i32.const 1
  owi: [INFO] stack         : [ 1 ; 10 ; 1 ]
  owi: [INFO] running instr : i32.add
  owi: [INFO] stack         : [ 11 ; 1 ]
  owi: [INFO] running instr : local.set 1
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : local.get 1
  owi: [INFO] stack         : [ 11 ; 1 ]
  owi: [INFO] running instr : i32.const 10
  owi: [INFO] stack         : [ 10 ; 11 ; 1 ]
  owi: [INFO] running instr : i32.le_s
  owi: [INFO] stack         : [ 0 ; 1 ]
  owi: [INFO] running instr : br_if 0
  owi: [INFO] stack         : [ 1 ]
  owi: [INFO] running instr : call 0
  owi: [INFO] stack         : [  ]
  owi: [INFO] running instr : call 1
  owi: [INFO] calling func  : func start
  owi: [INFO] Completed paths: 1
  All OK!

  $ owi iso a1.wat a2.wat --debug
  Module owi_iso_module1 is a1.wat
  Module owi_iso_module2 is a2.wat
  Compiling a1.wat
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  Compiling a2.wat
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  a1.wat exports: mul unused1
  a2.wat exports: mul unused2
  common exports: mul
  typechecking ...
  linking      ...
  typechecking ...
  linking      ...
  generated module:
    (module
      (import "owi_iso_module1" "mul" (func $iso_func1  (param $x i32) (param $y i32) (result i32)))
      (import "owi_iso_module2" "mul" (func $iso_func2  (param $x i32) (param $y i32) (result i32)))
      (import "symbolic" "assert" (func $assert  (param i32)))
      (import "symbolic" "i32_symbol" (func $i32_symbol  (result i32)))
      (import "symbolic" "i64_symbol" (func $i64_symbol  (result i64)))
      (import "symbolic" "f32_symbol" (func $f32_symbol  (result f32)))
      (import "symbolic" "f64_symbol" (func $f64_symbol  (result f64)))
      (func $check_iso_func (param $x i32) (param $y i32)
        local.get 0
        local.get 1
        call 0
        local.get 0
        local.get 1
        call 1
        i32.eq
        call 2
      )
      (func $start
        call 3
        call 3
        call 7
      )
      (start 8)
    )
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 8
  calling func : func start
  stack        : [  ]
  running instr: call 3
  stack        : [ symbol_0 ]
  running instr: call 3
  stack        : [ symbol_1 ; symbol_0 ]
  running instr: call 7
  calling func : func check_iso_func
  stack        : [  ]
  running instr: local.get 0
  stack        : [ symbol_0 ]
  running instr: local.get 1
  stack        : [ symbol_1 ; symbol_0 ]
  running instr: call 0
  calling func : func anonymous
  stack        : [  ]
  running instr: local.get 0
  stack        : [ symbol_0 ]
  running instr: local.get 1
  stack        : [ symbol_1 ; symbol_0 ]
  running instr: i32.mul
  stack        : [ (i32.mul symbol_0 symbol_1) ]
  stack        : [ (i32.mul symbol_0 symbol_1) ]
  running instr: local.get 0
  stack        : [ symbol_0 ; (i32.mul symbol_0 symbol_1) ]
  running instr: local.get 1
  stack        : [ symbol_1 ; symbol_0 ; (i32.mul symbol_0 symbol_1) ]
  running instr: call 1
  calling func : func anonymous
  stack        : [  ]
  running instr: local.get 1
  stack        : [ symbol_1 ]
  running instr: local.get 0
  stack        : [ symbol_0 ; symbol_1 ]
  running instr: i32.mul
  stack        : [ (i32.mul symbol_1 symbol_0) ]
  stack        : [ (i32.mul symbol_1 symbol_0) ; (i32.mul symbol_0 symbol_1) ]
  running instr: i32.eq
  stack        : [ (i32.of_bool
                    (bool.eq (i32.mul symbol_0 symbol_1)
                     (i32.mul symbol_1 symbol_0))) ]
  running instr: call 2
  stack        : [  ]
  stack        : [  ]
  stack        : [  ]
  Completed paths: 1
  All OK
  $ owi iso a1.wat a3.wat --debug
  Module owi_iso_module1 is a1.wat
  Module owi_iso_module2 is a3.wat
  Compiling a1.wat
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  Compiling a3.wat
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  a1.wat exports: mul unused1
  a3.wat exports: mul unused2
  common exports: mul
  typechecking ...
  linking      ...
  typechecking ...
  linking      ...
  generated module:
    (module
      (import "owi_iso_module1" "mul" (func $iso_func1  (param $x i32) (param $y i32) (result i32)))
      (import "owi_iso_module2" "mul" (func $iso_func2  (param $x i32) (param $y i32) (result i32)))
      (import "symbolic" "assert" (func $assert  (param i32)))
      (import "symbolic" "i32_symbol" (func $i32_symbol  (result i32)))
      (import "symbolic" "i64_symbol" (func $i64_symbol  (result i64)))
      (import "symbolic" "f32_symbol" (func $f32_symbol  (result f32)))
      (import "symbolic" "f64_symbol" (func $f64_symbol  (result f64)))
      (func $check_iso_func (param $x i32) (param $y i32)
        local.get 0
        local.get 1
        call 0
        local.get 0
        local.get 1
        call 1
        i32.eq
        call 2
      )
      (func $start
        call 3
        call 3
        call 7
      )
      (start 8)
    )
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 8
  calling func : func start
  stack        : [  ]
  running instr: call 3
  stack        : [ symbol_0 ]
  running instr: call 3
  stack        : [ symbol_1 ; symbol_0 ]
  running instr: call 7
  calling func : func check_iso_func
  stack        : [  ]
  running instr: local.get 0
  stack        : [ symbol_0 ]
  running instr: local.get 1
  stack        : [ symbol_1 ; symbol_0 ]
  running instr: call 0
  calling func : func anonymous
  stack        : [  ]
  running instr: local.get 0
  stack        : [ symbol_0 ]
  running instr: local.get 1
  stack        : [ symbol_1 ; symbol_0 ]
  running instr: i32.mul
  stack        : [ (i32.mul symbol_0 symbol_1) ]
  stack        : [ (i32.mul symbol_0 symbol_1) ]
  running instr: local.get 0
  stack        : [ symbol_0 ; (i32.mul symbol_0 symbol_1) ]
  running instr: local.get 1
  stack        : [ symbol_1 ; symbol_0 ; (i32.mul symbol_0 symbol_1) ]
  running instr: call 1
  calling func : func anonymous
  stack        : [  ]
  running instr: local.get 1
  stack        : [ symbol_1 ]
  running instr: local.get 0
  stack        : [ symbol_0 ; symbol_1 ]
  running instr: i32.mul
  stack        : [ (i32.mul symbol_1 symbol_0) ]
  running instr: i32.const 1
  stack        : [ 1 ; (i32.mul symbol_1 symbol_0) ]
  running instr: i32.add
  stack        : [ (i32.add (i32.mul symbol_1 symbol_0) 1) ]
  stack        : [ (i32.add (i32.mul symbol_1 symbol_0) 1) ; (i32.mul symbol_0
                                                              symbol_1) ]
  running instr: i32.eq
  stack        : [ (i32.of_bool
                    (bool.eq (i32.mul symbol_0 symbol_1)
                     (i32.add (i32.mul symbol_1 symbol_0) 1))) ]
  running instr: call 2
  Assert failure: (bool.eq (i32.mul symbol_0 symbol_1)
                   (i32.add (i32.mul symbol_1 symbol_0) 1))
  model {
    symbol symbol_0 i32 0
    symbol symbol_1 i32 0
  }
  Completed paths: 1
  Reached problem!
  [13]

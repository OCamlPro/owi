ref.null ref.is_null nop instructions:
  $ dune exec -- owi --debug --optimize ref_nop.wast
  parsing      ...
  simplifying  ...
  typechecking ...
  optimizing   ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 0
  calling func : func start
  stack        : [  ]
  running instr: i32.const 1
  stack        : [ i32.const 1 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

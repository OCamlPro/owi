f32.demote_f64 f64.promote_f32 instructions:
  $ dune exec -- owi --debug --optimize demote_promote.wast
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
  running instr: f32.const 42
  stack        : [ f32.const 42 ]
  running instr: drop
  stack        : [  ]
  running instr: f64.const 42
  stack        : [ f64.const 42 ]
  running instr: drop
  stack        : [  ]
  stack        : [  ]

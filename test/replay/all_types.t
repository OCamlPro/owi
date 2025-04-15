  $ owi sym -w1 all_types.wat > all_types.scfg
  Reached problem!
  [13]
  $ owi replay --replay-file all_types.scfg all_types.wat --debug
  parsing      ...
  checking     ...
  grouping     ...
  assigning    ...
  rewriting    ...
  typechecking ...
  typechecking ...
  linking      ...
  interpreting ...
  stack        : [  ]
  running instr: call 4
  calling func : func start
  stack        : [  ]
  running instr: i32.const 42
  stack        : [ i32.const 42 ]
  running instr: call 0
  stack        : [ i32.const 42 ; i32.const 42 ]
  running instr: i32.eq
  stack        : [ i32.const 1 ]
  running instr: if
  stack        : [  ]
  running instr: i64.const 84
  stack        : [ i64.const 84 ]
  running instr: call 2
  stack        : [ i64.const 84 ; i64.const 84 ]
  running instr: i64.eq
  stack        : [ i32.const 1 ]
  running instr: if
  stack        : [  ]
  running instr: f32.const 13.119_999_885_559_082
  stack        : [ f32.const 13.119_999_885_559_082 ]
  running instr: call 1
  stack        : [ f32.const 13.119_999_885_559_082 ; f32.const 13.119_999_885_559_082 ]
  running instr: f32.eq
  stack        : [ i32.const 1 ]
  running instr: if
  stack        : [  ]
  running instr: call 3
  stack        : [ f64.const 12.130_000_000_000_001 ]
  running instr: f64.const 12.130_000_000_000_001
  stack        : [ f64.const 12.130_000_000_000_001 ; f64.const 12.130_000_000_000_001 ]
  running instr: f64.eq
  stack        : [ i32.const 1 ]
  running instr: if
  stack        : [  ]
  running instr: unreachable
  unreachable
  [26]

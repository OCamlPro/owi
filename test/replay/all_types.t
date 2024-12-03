  $ owi sym -w1 all_types.wat > all_types.model
  Reached problem!
  [13]
  $ owi replay --replay-file all_types.model all_types.wat --debug
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
  running instr: (if
    (then
      i64.const 84
      call 2
      i64.eq
      (if
        (then
          f32.const 13.119_999_885_559_082
          call 1
          f32.eq
          (if
            (then
              call 3
              f64.const 12.130_000_000_000_001
              f64.eq
              (if
                (then
                  unreachable
                )
              )
            )
          )
        )
      )
    )
  )
  stack        : [  ]
  running instr: i64.const 84
  stack        : [ i64.const 84 ]
  running instr: call 2
  stack        : [ i64.const 84 ; i64.const 84 ]
  running instr: i64.eq
  stack        : [ i32.const 1 ]
  running instr: (if
    (then
      f32.const 13.119_999_885_559_082
      call 1
      f32.eq
      (if
        (then
          call 3
          f64.const 12.130_000_000_000_001
          f64.eq
          (if
            (then
              unreachable
            )
          )
        )
      )
    )
  )
  stack        : [  ]
  running instr: f32.const 13.119_999_885_559_082
  stack        : [ f32.const 13.119_999_885_559_082 ]
  running instr: call 1
  Got value i32.const 1095887749 but expected a f32 value.owi: internal error, uncaught exception:
                                                               File "src/cmd/cmd_replay.ml", line 59, characters 6-12: Assertion failed
                                                               Raised at Owi__Cmd_replay.run_file.symbol_f32 in file "src/cmd/cmd_replay.ml", line 59, characters 6-18
                                                               Called from Owi__Interpret.Make.exec_extern_func.apply in file "src/interpret/interpret.ml", line 529, characters 38-44
                                                               Called from Owi__Interpret.Make.exec_extern_func in file "src/interpret/interpret.ml", line 537, characters 13-45
                                                               Called from Owi__Interpret.Make.exec_vfunc in file "src/interpret/interpret.ml", line 765, characters 19-59
                                                               Called from Owi__Interpret.Make.loop in file "src/interpret/interpret.ml", line 1461, characters 19-53
                                                               Called from Owi__Interpret.Make.exec_expr in file "src/interpret/interpret.ml", line 1489, characters 17-27
                                                               Called from Owi__Interpret.Make.modul.(fun) in file "src/interpret/interpret.ml", lines 1503-1504, characters 16-22
                                                               Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
                                                               Called from Owi__Interpret.Make.modul in file "src/interpret/interpret.ml", lines 1498-1511, characters 10-40
                                                               Called from Owi__Cmd_replay.cmd in file "src/cmd/cmd_replay.ml", line 144, characters 12-49
                                                               Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
                                                               Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [125]

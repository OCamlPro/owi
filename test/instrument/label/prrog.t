  $ owi instrument label --criteria=fc prog.wat
  owi: internal error, uncaught exception:
       File "src/instrument/label.ml", line 26, characters 21-27: Assertion failed
       Raised at Owi__Label.annotate_fc in file "src/instrument/label.ml", line 26, characters 21-33
       Called from Owi__Cmd_instrument_label.cmd in file "src/cmd/cmd_instrument_label.ml", line 15, characters 10-44
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [125]
  $ cat prog.instrumented.wat
  cat: prog.instrumented.wat: No such file or directory
  [1]
  $ owi instrument label --criteria=sc prog.wat
  owi: internal error, uncaught exception:
       File "src/instrument/label.ml", line 28, characters 21-27: Assertion failed
       Raised at Owi__Label.annotate_sc in file "src/instrument/label.ml", line 28, characters 21-33
       Called from Owi__Cmd_instrument_label.cmd in file "src/cmd/cmd_instrument_label.ml", line 15, characters 10-44
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [125]
  $ cat prog.instrumented.wat
  cat: prog.instrumented.wat: No such file or directory
  [1]
  $ owi instrument label --criteria=dc prog.wat
  owi: internal error, uncaught exception:
       File "src/instrument/label.ml", line 30, characters 21-27: Assertion failed
       Raised at Owi__Label.annotate_dc in file "src/instrument/label.ml", line 30, characters 21-33
       Called from Owi__Cmd_instrument_label.cmd in file "src/cmd/cmd_instrument_label.ml", line 15, characters 10-44
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [125]
  $ cat prog.instrumented.wat
  cat: prog.instrumented.wat: No such file or directory
  [1]

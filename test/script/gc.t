  $ owi script --no-exhaustion reference/array.wast
  owi: [ERROR] unexpected token "i8" in line 4, character 15-17
  [40]
  $ owi script --no-exhaustion reference/br_on_cast_fail.wast
  owi: [ERROR] unexpected token "(" in line 5, character 20-21
  [40]
  $ owi script --no-exhaustion reference/br_on_cast.wast
  owi: [ERROR] unexpected token "(" in line 5, character 20-21
  [40]
  $ owi script --no-exhaustion reference/call_ref.wast
  owi: internal error, uncaught exception:
       File "src/interpret/interpret.ml", line 853, characters 4-10: Assertion failed
       Raised at Owi__Interpret.Make.call_ref in file "src/interpret/interpret.ml" (inlined), line 853, characters 4-16
       Called from Owi__Interpret.Make.exec_instr in file "src/interpret/interpret.ml", line 1612, characters 24-58
       Called from Owi__Interpret.Make.loop in file "src/interpret/interpret.ml", line 1626, characters 19-53
       Called from Owi__Interpret.Make.exec_vfunc_from_outside in file "src/interpret/interpret.ml", lines 1715-1732, characters 6-9
       Called from Owi__Script.run.(fun) in file "src/script/script.ml", line 304, characters 21-40
       Called from Owi__Syntax.list_fold_left.aux in file "src/infra/syntax.ml", line 42, characters 12-19
       Called from Owi__Script.exec in file "src/script/script.ml", line 351, characters 21-46
       Called from Owi__Syntax.list_iter.aux in file "src/infra/syntax.ml", line 16, characters 25-28
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [125]
  $ owi script --no-exhaustion reference/extern.wast
  owi: [ERROR] unexpected token ")" in line 3, character 19-20
  [40]
  $ owi script --no-exhaustion reference/i31.wast
  owi: [ERROR] unexpected token ")" in line 2, character 54-55
  [40]
  $ owi script --no-exhaustion reference/ref_cast.wast
  owi: [ERROR] unexpected token ")" in line 5, character 19-20
  [40]
  $ true # TODO: enable again: owi script --no-exhaustion reference/ref_eq.wast
  $ owi script --no-exhaustion reference/ref_test.wast
  owi: [ERROR] unexpected token ")" in line 5, character 19-20
  [40]
  $ owi script --no-exhaustion reference/return_call_ref.wast
  owi: internal error, uncaught exception:
       File "src/interpret/interpret.ml", line 853, characters 4-10: Assertion failed
       Raised at Owi__Interpret.Make.call_ref in file "src/interpret/interpret.ml" (inlined), line 853, characters 4-16
       Called from Owi__Interpret.Make.exec_instr in file "src/interpret/interpret.ml", line 1613, characters 31-64
       Called from Owi__Interpret.Make.loop in file "src/interpret/interpret.ml", line 1626, characters 19-53
       Called from Owi__Interpret.Make.exec_vfunc_from_outside in file "src/interpret/interpret.ml", lines 1715-1732, characters 6-9
       Called from Owi__Script.run.(fun) in file "src/script/script.ml", line 304, characters 21-40
       Called from Owi__Syntax.list_fold_left.aux in file "src/infra/syntax.ml", line 42, characters 12-19
       Called from Owi__Script.exec in file "src/script/script.ml", line 351, characters 21-46
       Called from Owi__Syntax.list_iter.aux in file "src/infra/syntax.ml", line 16, characters 25-28
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [125]
  $ owi script --no-exhaustion reference/struct.wast
  owi: [ERROR] unexpected token ")" in line 4, character 15-16
  [40]
  $ owi script --no-exhaustion reference/type-subtyping.wast
  owi: [ERROR] unexpected token "(" in line 4, character 17-18
  [40]

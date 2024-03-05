  $ owi script --no-exhaustion gc/array.wast
  no error
  $ owi script --no-exhaustion gc/br_on_cast_fail.wast
  unexpected token
  [40]
  $ owi script --no-exhaustion gc/br_on_cast.wast
  unexpected token
  [40]
  $ owi script --no-exhaustion gc/call_ref.wast
  unknown type
  [49]
  $ owi script --no-exhaustion gc/extern.wast
  owi: internal error, uncaught exception:
       File "src/typecheck.ml", line 478, characters 4-10: Assertion failed
       Raised at Owi__Typecheck.typecheck_instr in file "src/typecheck.ml", line 478, characters 4-16
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Typecheck.typecheck_expr in file "src/typecheck.ml", line 490, characters 15-59
       Called from Owi__Typecheck.typecheck_function in file "src/typecheck.ml", line 508, characters 6-112
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Compile.until_typecheck in file "src/compile.ml", line 28, characters 14-31
       Called from Owi__Compile.until_optimize in file "src/compile.ml", line 32, characters 11-36
       Called from Owi__Compile.until_link in file "src/compile.ml", line 36, characters 11-45
       Called from Owi__Compile.until_interpret in file "src/compile.ml", line 40, characters 23-70
       Called from Owi__Script.run.(fun) in file "src/script.ml", line 152, characters 10-75
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Script.exec in file "src/script.ml", line 253, characters 21-56
       Called from Owi__Syntax.list_iter.(fun) in file "src/syntax.ml", line 22, characters 14-17
       Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
       Called from Owi__Syntax.list_iter in file "src/syntax.ml", line 20, characters 4-157
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
  [125]
  $ owi script --no-exhaustion gc/i31.wast
  owi: internal error, uncaught exception:
       File "src/script.ml", line 103, characters 4-10: Assertion failed
       Raised at Owi__Script.compare_result_const in file "src/script.ml", line 103, characters 4-16
       Called from Stdlib__List.for_all2 in file "list.ml", line 181, characters 24-31
       Called from Owi__Script.run.(fun) in file "src/script.ml", line 219, characters 17-74
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Script.exec in file "src/script.ml", line 253, characters 21-56
       Called from Owi__Syntax.list_iter.(fun) in file "src/syntax.ml", line 22, characters 14-17
       Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
       Called from Owi__Syntax.list_iter in file "src/syntax.ml", line 20, characters 4-157
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
  [125]
  $ owi script --no-exhaustion gc/ref_cast.wast
  owi: internal error, uncaught exception:
       File "src/typecheck.ml", line 478, characters 4-10: Assertion failed
       Raised at Owi__Typecheck.typecheck_instr in file "src/typecheck.ml", line 478, characters 4-16
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Typecheck.typecheck_expr in file "src/typecheck.ml", line 490, characters 15-59
       Called from Owi__Typecheck.typecheck_function in file "src/typecheck.ml", line 508, characters 6-112
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Compile.until_typecheck in file "src/compile.ml", line 28, characters 14-31
       Called from Owi__Compile.until_optimize in file "src/compile.ml", line 32, characters 11-36
       Called from Owi__Compile.until_link in file "src/compile.ml", line 36, characters 11-45
       Called from Owi__Compile.until_interpret in file "src/compile.ml", line 40, characters 23-70
       Called from Owi__Script.run.(fun) in file "src/script.ml", line 152, characters 10-75
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Script.exec in file "src/script.ml", line 253, characters 21-56
       Called from Owi__Syntax.list_iter.(fun) in file "src/syntax.ml", line 22, characters 14-17
       Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
       Called from Owi__Syntax.list_iter in file "src/syntax.ml", line 20, characters 4-157
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
  [125]
  $ owi script --no-exhaustion gc/ref_eq.wast
  owi: internal error, uncaught exception:
       File "src/typecheck.ml", line 478, characters 4-10: Assertion failed
       Raised at Owi__Typecheck.typecheck_instr in file "src/typecheck.ml", line 478, characters 4-16
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Typecheck.typecheck_expr in file "src/typecheck.ml", line 490, characters 15-59
       Called from Owi__Typecheck.typecheck_function in file "src/typecheck.ml", line 508, characters 6-112
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Compile.until_typecheck in file "src/compile.ml", line 28, characters 14-31
       Called from Owi__Compile.until_optimize in file "src/compile.ml", line 32, characters 11-36
       Called from Owi__Compile.until_link in file "src/compile.ml", line 36, characters 11-45
       Called from Owi__Compile.until_interpret in file "src/compile.ml", line 40, characters 23-70
       Called from Owi__Script.run.(fun) in file "src/script.ml", line 152, characters 10-75
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Script.exec in file "src/script.ml", line 253, characters 21-56
       Called from Owi__Syntax.list_iter.(fun) in file "src/syntax.ml", line 22, characters 14-17
       Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
       Called from Owi__Syntax.list_iter in file "src/syntax.ml", line 20, characters 4-157
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
  [125]
  $ owi script --no-exhaustion gc/ref_test.wast
  owi: internal error, uncaught exception:
       File "src/typecheck.ml", line 478, characters 4-10: Assertion failed
       Raised at Owi__Typecheck.typecheck_instr in file "src/typecheck.ml", line 478, characters 4-16
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Typecheck.typecheck_expr in file "src/typecheck.ml", line 490, characters 15-59
       Called from Owi__Typecheck.typecheck_function in file "src/typecheck.ml", line 508, characters 6-112
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Compile.until_typecheck in file "src/compile.ml", line 28, characters 14-31
       Called from Owi__Compile.until_optimize in file "src/compile.ml", line 32, characters 11-36
       Called from Owi__Compile.until_link in file "src/compile.ml", line 36, characters 11-45
       Called from Owi__Compile.until_interpret in file "src/compile.ml", line 40, characters 23-70
       Called from Owi__Script.run.(fun) in file "src/script.ml", line 152, characters 10-75
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Script.exec in file "src/script.ml", line 253, characters 21-56
       Called from Owi__Syntax.list_iter.(fun) in file "src/syntax.ml", line 22, characters 14-17
       Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
       Called from Owi__Syntax.list_iter in file "src/syntax.ml", line 20, characters 4-157
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
  [125]
  $ owi script --no-exhaustion gc/return_call_ref.wast
  unknown type
  [49]
  $ owi script --no-exhaustion gc/struct.wast
  no error
  $ owi script --no-exhaustion gc/type-subtyping.wast
  unknown type
  [49]

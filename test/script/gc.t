  $ owi script --no-exhaustion reference/proposals/gc/array.wast
  unknown operator unknown operator "array.get_s"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast_fail.wast
  unknown operator unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast.wast
  unknown operator unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/call_ref.wast
  unknown type
  [49]
  $ owi script --no-exhaustion reference/proposals/gc/extern.wast
  unknown operator unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/i31.wast
  unknown operator unknown operator "i31ref"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/ref_cast.wast
  unknown operator unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/ref_eq.wast
  owi: internal error, uncaught exception:
       File "src/typecheck/typecheck.ml", line 478, characters 4-10: Assertion failed
       Raised at Owi__Typecheck.typecheck_instr in file "src/typecheck/typecheck.ml", line 478, characters 4-16
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Typecheck.typecheck_expr in file "src/typecheck/typecheck.ml", line 490, characters 15-59
       Called from Owi__Typecheck.typecheck_function in file "src/typecheck/typecheck.ml", line 512, characters 6-112
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Compile.Text.until_typecheck in file "src/compile.ml", line 26, characters 16-33
       Called from Owi__Compile.Text.until_optimize in file "src/compile.ml", line 30, characters 13-38
       Called from Owi__Compile.Text.until_link in file "src/compile.ml", line 34, characters 13-47
       Called from Owi__Compile.Text.until_interpret in file "src/compile.ml", line 38, characters 25-72
       Called from Owi__Script.run.(fun) in file "src/script/script.ml", line 152, characters 10-80
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Script.exec in file "src/script/script.ml", line 260, characters 21-56
       Called from Owi__Syntax.list_iter.(fun) in file "src/utils/syntax.ml", line 22, characters 14-17
       Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
       Called from Owi__Syntax.list_iter in file "src/utils/syntax.ml", line 20, characters 4-157
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 34, characters 37-44
  [125]
  $ owi script --no-exhaustion reference/proposals/gc/ref_test.wast
  unknown operator unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/return_call_ref.wast
  unknown type
  [49]
  $ owi script --no-exhaustion reference/proposals/gc/struct.wast
  unknown operator unknown operator "struct.get_u"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/type-subtyping.wast
  unexpected token "ref"
  [40]

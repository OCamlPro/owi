  $ owi script --no-exhaustion reference/proposals/gc/array.wast
  unknown operator "array.get_s"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast_fail.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/br_on_cast.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/call_ref.wast
  unknown type $ii
  [52]
  $ owi script --no-exhaustion reference/proposals/gc/extern.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/i31.wast
  unknown operator "i31ref"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/ref_cast.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/ref_eq.wast
  owi: internal error, uncaught exception:
       File "src/ast/types.ml", line 930, characters 12-18: Assertion failed
       Raised at Owi__Types.compare_str_type in file "src/ast/types.ml", line 930, characters 12-24
       Called from Stdlib__Map.Make.add in file "map.ml", line 129, characters 18-33
       Called from Owi__Assigned.assign_type in file "src/text_to_binary/assigned.ml", line 54, characters 20-53
       Called from Owi__Syntax.let+ in file "src/utils/syntax.ml", line 9, characters 43-48
       Called from Owi__Assigned.assign_type in file "src/text_to_binary/assigned.ml", lines 45-55, characters 4-63
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Assigned.assign_types in file "src/text_to_binary/assigned.ml", line 88, characters 13-70
       Called from Owi__Assigned.of_grouped in file "src/text_to_binary/assigned.ml", line 132, characters 13-31
       Called from Owi__Compile.Text.until_binary in file "src/ast/compile.ml", line 20, characters 13-35
       Called from Owi__Compile.Text.until_binary_validate in file "src/ast/compile.ml", line 27, characters 13-46
       Called from Owi__Compile.Text.until_optimize in file "src/ast/compile.ml", line 34, characters 13-55
       Called from Owi__Compile.Text.until_link in file "src/ast/compile.ml", line 38, characters 13-58
       Called from Owi__Compile.Text.until_interpret in file "src/ast/compile.ml", line 43, characters 6-64
       Called from Owi__Script.run.(fun) in file "src/script/script.ml", lines 160-161, characters 10-34
       Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
       Called from Owi__Script.exec in file "src/script/script.ml", line 298, characters 21-56
       Called from Owi__Syntax.list_iter.(fun) in file "src/utils/syntax.ml", line 18, characters 14-17
       Called from Stdlib__List.iter in file "list.ml", line 112, characters 12-15
       Called from Owi__Syntax.list_iter in file "src/utils/syntax.ml", lines 16-23, characters 4-7
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 24, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 35, characters 37-44
  [125]
  $ owi script --no-exhaustion reference/proposals/gc/ref_test.wast
  unknown operator "any.convert_extern"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/return_call_ref.wast
  unknown type $i64-i64
  [52]
  $ owi script --no-exhaustion reference/proposals/gc/struct.wast
  unknown operator "struct.get_u"
  [23]
  $ owi script --no-exhaustion reference/proposals/gc/type-subtyping.wast
  unexpected token "ref"
  [40]

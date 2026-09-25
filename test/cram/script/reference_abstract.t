$ owi wasm script abstract --no-exhaustion reference/address.wast
$ owi wasm script abstract --no-exhaustion reference/align.wast
$ owi wasm script abstract --no-exhaustion reference/binary-leb128.wast
  $ owi wasm script abstract --no-exhaustion reference/block.wast
  owi: [ERROR] File "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-24: Assertion failed
  Exception: File "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-24: Assertion failed
  Raised at Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-30
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.exec_vfunc_from_outside in file "src/abstract/abstract_interpreter_control_flow.ml", line 631, characters 12-62
  Called from Owi__Script_abstract.do_action in file "src/script/script_abstract.ml", line 23, characters 8-52
  [26]
  $ owi wasm script abstract --no-exhaustion reference/br_if.wast
  $ owi wasm script abstract --no-exhaustion reference/br_table.wast
  owi: [ERROR] failed
  [26]
  $ owi wasm script abstract --no-exhaustion reference/br.wast
$ owi wasm script abstract --no-exhaustion reference/bulk.wast
$ owi wasm script abstract --no-exhaustion reference/call_indirect.wast
  $ owi wasm script abstract --no-exhaustion reference/call.wast
  owi: [ERROR] Owi__Abstract_interpreter_control_flow.RecursiveFunctionCall
  Exception: Owi__Abstract_interpreter_control_flow.RecursiveFunctionCall
  Raised at Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 285, characters 53-80
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 380, characters 18-53
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 438-440, characters 10-66
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.exec_vfunc_from_outside in file "src/abstract/abstract_interpreter_control_flow.ml", line 631, characters 12-62
  Called from Owi__Script_abstract.do_action in file "src/script/script_abstract.ml", line 23, characters 8-52
  [26]
  $ owi wasm script abstract --no-exhaustion reference/comments.wast
$ owi wasm script abstract --no-exhaustion reference/const.wast
  $ owi wasm script abstract --no-exhaustion reference/conversions.wast
  owi: [ERROR] File "src/abstract/abstract_stack.ml", line 58, characters 46-52: Assertion failed
  Exception: File "src/abstract/abstract_stack.ml", line 58, characters 46-52: Assertion failed
  Raised at Owi__Abstract_stack.pop_i32 in file "src/abstract/abstract_stack.ml", line 58, characters 46-58
  Called from Owi__Abstract_stack.apply_i32_i32 in file "src/abstract/abstract_stack.ml", line 138, characters 15-24
  Called from Owi__Abstract_interpreter_simple.eval_i32 in file "src/abstract/abstract_interpreter_simple.ml", line 179, characters 16-69
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 585, characters 18-58
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.exec_vfunc_from_outside in file "src/abstract/abstract_interpreter_control_flow.ml", line 631, characters 12-62
  Called from Owi__Script_abstract.do_action in file "src/script/script_abstract.ml", line 23, characters 8-52
  [26]
$ owi wasm script abstract --no-exhaustion reference/custom.wast
$ owi wasm script abstract --no-exhaustion reference/elem.wast
$ owi wasm script abstract --no-exhaustion reference/endianness.wast
$ owi wasm script abstract --no-exhaustion reference/exports.wast
$ owi wasm script abstract --no-exhaustion reference/f32_bitwise.wast
$ owi wasm script abstract --no-exhaustion reference/f32_cmp.wast
$ owi wasm script abstract --no-exhaustion reference/f32.wast
$ owi wasm script abstract --no-exhaustion reference/f64_bitwise.wast
$ owi wasm script abstract --no-exhaustion reference/f64_cmp.wast
$ owi wasm script abstract --no-exhaustion reference/f64.wast
$ owi wasm script abstract --no-exhaustion reference/fac.wast
$ owi wasm script abstract --no-exhaustion reference/float_exprs.wast
$ owi wasm script abstract --no-exhaustion reference/float_literals.wast
$ owi wasm script abstract --no-exhaustion reference/float_memory.wast
$ owi wasm script abstract --no-exhaustion reference/float_misc.wast
$ owi wasm script abstract --no-exhaustion reference/forward.wast
$ owi wasm script abstract --no-exhaustion reference/func_ptrs.wast
  $ owi wasm script abstract --no-exhaustion reference/func.wast
  owi: [ERROR] failed
  [26]
  $ owi wasm script abstract --no-exhaustion reference/global.wast
  owi: [ERROR] got:      [ref ...] expected: (ref.null extern)
  owi: [ERROR] bad result
  [3]
  $ owi wasm script abstract --no-exhaustion reference/i32.wast
  owi: [WARNING] (assert_trap (invoke "div_s" (i32.const 1) (i32.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "div_s" (i32.const 0) (i32.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "div_s" (i32.const -2147483648) (i32.const -1)) "integer overflow") is not handled
  owi: [WARNING] (assert_trap (invoke "div_s" (i32.const -2147483648) (i32.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "div_u" (i32.const 1) (i32.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "div_u" (i32.const 0) (i32.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "rem_s" (i32.const 1) (i32.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "rem_s" (i32.const 0) (i32.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "rem_u" (i32.const 1) (i32.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "rem_u" (i32.const 0) (i32.const 0)) "integer divide by zero") is not handled
  $ owi wasm script abstract --no-exhaustion reference/i64.wast
  owi: [WARNING] (assert_trap (invoke "div_s" (i64.const 1) (i64.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "div_s" (i64.const 0) (i64.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "div_s" (i64.const -9223372036854775808) (i64.const -1)) "integer overflow") is not handled
  owi: [WARNING] (assert_trap (invoke "div_s" (i64.const -9223372036854775808) (i64.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "div_u" (i64.const 1) (i64.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "div_u" (i64.const 0) (i64.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "rem_s" (i64.const 1) (i64.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "rem_s" (i64.const 0) (i64.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "rem_u" (i64.const 1) (i64.const 0)) "integer divide by zero") is not handled
  owi: [WARNING] (assert_trap (invoke "rem_u" (i64.const 0) (i64.const 0)) "integer divide by zero") is not handled
  $ owi wasm script abstract --no-exhaustion reference/if.wast
  owi: [ERROR] File "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-24: Assertion failed
  Exception: File "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-24: Assertion failed
  Raised at Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-30
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.exec_vfunc_from_outside in file "src/abstract/abstract_interpreter_control_flow.ml", line 631, characters 12-62
  Called from Owi__Script_abstract.do_action in file "src/script/script_abstract.ml", line 23, characters 8-52
  [26]
  $ owi wasm script abstract --no-exhaustion reference/inline-module.wast
  $ owi wasm script abstract --no-exhaustion reference/int_exprs.wast
  owi: [ERROR] File "src/abstract/abstract_stack.ml", line 58, characters 46-52: Assertion failed
  Exception: File "src/abstract/abstract_stack.ml", line 58, characters 46-52: Assertion failed
  Raised at Owi__Abstract_stack.pop_i32 in file "src/abstract/abstract_stack.ml", line 58, characters 46-58
  Called from Owi__Abstract_stack.apply_i32_i32 in file "src/abstract/abstract_stack.ml", line 138, characters 15-24
  Called from Owi__Abstract_interpreter_simple.eval_i32 in file "src/abstract/abstract_interpreter_simple.ml", line 179, characters 16-69
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 585, characters 18-58
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.exec_vfunc_from_outside in file "src/abstract/abstract_interpreter_control_flow.ml", line 631, characters 12-62
  Called from Owi__Script_abstract.do_action in file "src/script/script_abstract.ml", line 23, characters 8-52
  [26]
  $ owi wasm script abstract --no-exhaustion reference/int_literals.wast
  owi: [ERROR] expected unknown operator but got (unexpected token "i32" in line 1, character 8-11)
  [55]
$ owi wasm script abstract --no-exhaustion reference/labels.wast
$ owi wasm script abstract --no-exhaustion reference/left-to-right.wast
  $ owi wasm script abstract --no-exhaustion reference/linking.wast
  owi: [WARNING] (assert_unlinkable
                   (module
                     (import "reexport_f" "print" (func  (param i64)))
                   )
                   "incompatible import type"
                 ) is not handled
  owi: [WARNING] (assert_unlinkable
                   (module
                     (import "reexport_f" "print" (func  (param i32) (result i32)))
                   )
                   "incompatible import type"
                 ) is not handled
  owi: internal error, uncaught exception:
       File "src/script/script_abstract.ml", line 34, characters 4-10: Assertion failed
       Raised at Owi__Script_abstract.do_action in file "src/script/script_abstract.ml", line 34, characters 4-16
       Called from Stdlib__Result.map in file "result.ml", line 27, characters 32-37
       Called from Owi__Script_abstract.run_one in file "src/script/script_abstract.ml", line 131, characters 17-37
       Called from Stdlib__List.fold_left in file "list.ml", line 125, characters 24-34
       Called from Owi__Script_abstract.exec in file "src/script/script_abstract.ml", line 196, characters 12-37
       Called from Owi__Syntax.list_iter.aux in file "src/infra/syntax.ml", line 11, characters 25-28
       Called from Owi__Cmd_wasm_script.cmd_abstract in file "src/cmd/cmd_wasm_script.ml", line 22, characters 15-46
       Called from Cmdliner_term.app.(fun) in file "cmdliner_term.ml", line 22, characters 19-24
       Called from Cmdliner_eval.run_parser in file "cmdliner_eval.ml", line 41, characters 7-16
  [125]
  $ owi wasm script abstract --no-exhaustion reference/local_get.wast
  $ owi wasm script abstract --no-exhaustion reference/local_set.wast
  $ owi wasm script abstract --no-exhaustion reference/local_tee.wast
  owi: [ERROR] failed
  [26]
  $ owi wasm script abstract --no-exhaustion reference/loop.wast
  owi: [ERROR] Failure("join on stacks of different sizes")
  Exception: Failure("join on stacks of different sizes")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Owi__Abstract_interpreter_control_flow.serialize.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", lines 111-113, characters 4-58
  Called from Owi__Abstract_interpreter_control_flow.widen in file "src/abstract/abstract_interpreter_control_flow.ml", line 175, characters 4-42
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun).fixpoint in file "src/abstract/abstract_interpreter_control_flow.ml", line 496, characters 34-72
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 517, characters 31-45
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.exec_vfunc_from_outside in file "src/abstract/abstract_interpreter_control_flow.ml", line 631, characters 12-62
  Called from Owi__Script_abstract.do_action in file "src/script/script_abstract.ml", line 23, characters 8-52
  [26]
$ owi wasm script abstract --no-exhaustion reference/memory_copy.wast
$ owi wasm script abstract --no-exhaustion reference/memory_fill.wast
$ owi wasm script abstract --no-exhaustion reference/memory_init.wast
$ owi wasm script abstract --no-exhaustion reference/memory_redundancy.wast
$ owi wasm script abstract --no-exhaustion reference/memory_trap.wast
$ owi wasm script abstract --no-exhaustion reference/names.wast
  $ owi wasm script abstract --no-exhaustion reference/nop.wast
  owi: [ERROR] File "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-24: Assertion failed
  Exception: File "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-24: Assertion failed
  Raised at Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 598, characters 18-30
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_instr.(fun) in file "src/abstract/abstract_interpreter_control_flow.ml", line 396, characters 27-47
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_expr.loop in file "src/abstract/abstract_interpreter_control_flow.ml", line 265, characters 32-54
  Called from Owi__Abstract_interpreter_control_flow.DenotFixpoint.eval_func in file "src/abstract/abstract_interpreter_control_flow.ml", line 321, characters 27-55
  Called from Owi__Abstract_interpreter_control_flow.exec_vfunc_from_outside in file "src/abstract/abstract_interpreter_control_flow.ml", line 631, characters 12-62
  Called from Owi__Script_abstract.do_action in file "src/script/script_abstract.ml", line 23, characters 8-52
  [26]
$ owi wasm script abstract --no-exhaustion reference/ref_as_non_null.wast
$ owi wasm script abstract --no-exhaustion reference/ref.wast
$ owi wasm script abstract --no-exhaustion reference/ref_func.wast
$ owi wasm script abstract --no-exhaustion reference/ref_is_null.wast
$ owi wasm script abstract --no-exhaustion reference/ref_null.wast
$ owi wasm script abstract --no-exhaustion reference/return_call_indirect.wast
$ owi wasm script abstract --no-exhaustion reference/return_call.wast
  $ owi wasm script abstract --no-exhaustion reference/return.wast
  $ owi wasm script abstract --no-exhaustion reference/select.wast
  owi: [ERROR] got:      [ref ...] expected: (ref.null func)
  owi: [ERROR] bad result
  [3]
$ owi wasm script abstract --no-exhaustion reference/skip-stack-guard-page.wast
$ owi wasm script abstract --no-exhaustion reference/simd_address.wast
$ owi wasm script abstract --no-exhaustion reference/simd_align.wast
$ owi wasm script abstract --no-exhaustion reference/simd_bit_shift.wast
$ owi wasm script abstract --no-exhaustion reference/simd_bitwise.wast
$ owi wasm script abstract --no-exhaustion reference/simd_boolean.wast
$ owi wasm script abstract --no-exhaustion reference/simd_const.wast
$ owi wasm script abstract --no-exhaustion reference/simd_conversions.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f32x4_arith.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f32x4_cmp.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f32x4_pmin_pmax.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f32x4_rounding.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f32x4.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f64x2_arith.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f64x2_cmp.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f64x2_pmin_pmax.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f64x2_rounding.wast
$ owi wasm script abstract --no-exhaustion reference/simd_f64x2.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i16x8_arith2.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i16x8_arith.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i16x8_cmp.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i16x8_extadd_pairwise_i8x16.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i16x8_extmul_i8x16.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i16x8_q15mulr_sat_s.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i16x8_sat_arith.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i32x4_arith2.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i32x4_arith.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i32x4_cmp.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i32x4_dot_i16x8.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i32x4_extadd_pairwise_i16x8.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i32x4_extmul_i16x8.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i32x4_trunc_sat_f32x4.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i32x4_trunc_sat_f64x2.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i64x2_arith2.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i64x2_arith.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i64x2_cmp.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i64x2_extmul_i32x4.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i8x16_arith2.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i8x16_arith.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i8x16_cmp.wast
$ owi wasm script abstract --no-exhaustion reference/simd_i8x16_sat_arith.wast
$ owi wasm script abstract --no-exhaustion reference/simd_int_to_int_extend.wast
$ owi wasm script abstract --no-exhaustion reference/simd_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_linking.wast
$ owi wasm script abstract --no-exhaustion reference/simd_load16_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_load32_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_load64_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_load8_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_load_extend.wast
$ owi wasm script abstract --no-exhaustion reference/simd_load_splat.wast
$ owi wasm script abstract --no-exhaustion reference/simd_load.wast
$ owi wasm script abstract --no-exhaustion reference/simd_load_zero.wast
$ owi wasm script abstract --no-exhaustion reference/simd_memory-multi.wast
$ owi wasm script abstract --no-exhaustion reference/simd_select.wast
$ owi wasm script abstract --no-exhaustion reference/simd_splat.wast
$ owi wasm script abstract --no-exhaustion reference/simd_store16_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_store32_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_store64_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_store8_lane.wast
$ owi wasm script abstract --no-exhaustion reference/simd_store.wast
$ owi wasm script abstract --no-exhaustion reference/start.wast
$ owi wasm script abstract --no-exhaustion reference/switch.wast
$ owi wasm script abstract --no-exhaustion reference/table_copy.wast
$ owi wasm script abstract --no-exhaustion reference/table_fill.wast
$ owi wasm script abstract --no-exhaustion reference/table_get.wast
$ owi wasm script abstract --no-exhaustion reference/table_grow.wast
$ owi wasm script abstract --no-exhaustion reference/table_init.wast
$ owi wasm script abstract --no-exhaustion reference/table_set.wast
$ owi wasm script abstract --no-exhaustion reference/table_size.wast
$ owi wasm script abstract --no-exhaustion reference/table-sub.wast
$ owi wasm script abstract --no-exhaustion reference/table.wast
$ owi wasm script abstract --no-exhaustion reference/token.wast
$ owi wasm script abstract --no-exhaustion reference/token.wast
$ owi wasm script abstract --no-exhaustion reference/traps.wast
$ owi wasm script abstract --no-exhaustion reference/type.wast
  $ owi wasm script abstract --no-exhaustion reference/unreachable.wast
  owi: [WARNING] (assert_trap (invoke "type-i32" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "type-i64" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "type-f32" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "type-f64" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-func-first" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-func-mid" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-func-last" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-func-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-block-first" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-block-mid" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-block-last" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-block-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-loop-first" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-loop-mid" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-loop-last" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br_if-cond" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br_if-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br_if-value-cond" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br_table-index" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br_table-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br_table-value-2" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br_table-value-index" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-br_table-value-and-index" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-return-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-if-cond" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-if-then" (i32.const 1) (i32.const 6)) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-if-else" (i32.const 0) (i32.const 6)) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-if-then-no-else" (i32.const 1) (i32.const 6)) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-select-first" (i32.const 0) (i32.const 6)) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-select-first" (i32.const 1) (i32.const 6)) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-select-second" (i32.const 0) (i32.const 6)) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-select-second" (i32.const 1) (i32.const 6)) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-select-cond" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-call-first" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-call-mid" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-call-last" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-call_indirect-func" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-call_indirect-first" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-call_indirect-mid" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-call_indirect-last" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-local.set-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-local.tee-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-global.set-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-load-address" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-loadN-address" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-store-address" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-store-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-storeN-address" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-storeN-value" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-unary-operand" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-binary-left" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-binary-right" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-test-operand" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-compare-left" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-compare-right" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-convert-operand" ) "unreachable") is not handled
  owi: [WARNING] (assert_trap (invoke "as-memory.grow-size" ) "unreachable") is not handled
$ owi wasm script abstract --no-exhaustion reference/unreached-invalid.wast
$ owi wasm script abstract --no-exhaustion reference/unreached-valid.wast
$ owi wasm script abstract --no-exhaustion reference/unwind.wast
$ owi wasm script abstract --no-exhaustion reference/utf8-custom-section-id.wast
$ owi wasm script abstract --no-exhaustion reference/utf8-import-field.wast
$ owi wasm script abstract --no-exhaustion reference/utf8-import-module.wast
$ owi wasm script abstract --no-exhaustion reference/utf8-invalid-encoding.wast
$ owi wasm script abstract --no-exhaustion reference/annotations.wast
$ owi wasm script abstract --no-exhaustion reference/address0.wast
$ owi wasm script abstract --no-exhaustion reference/address1.wast
$ owi wasm script abstract --no-exhaustion reference/align0.wast
$ owi wasm script abstract --no-exhaustion reference/binary.wast
$ owi wasm script abstract --no-exhaustion reference/binary0.wast
$ owi wasm script abstract --no-exhaustion reference/data_drop0.wast
$ owi wasm script abstract --no-exhaustion reference/data.wast
$ owi wasm script abstract --no-exhaustion reference/data0.wast
$ owi wasm script abstract --no-exhaustion reference/data1.wast
$ owi wasm script abstract --no-exhaustion reference/exports0.wast
$ owi wasm script abstract --no-exhaustion reference/float_exprs0.wast
$ owi wasm script abstract --no-exhaustion reference/float_exprs1.wast
$ owi wasm script abstract --no-exhaustion reference/float_memory0.wast
$ owi wasm script abstract --no-exhaustion reference/imports.wast
$ owi wasm script abstract --no-exhaustion reference/imports0.wast
$ owi wasm script abstract --no-exhaustion reference/imports1.wast
$ owi wasm script abstract --no-exhaustion reference/imports2.wast
$ owi wasm script abstract --no-exhaustion reference/imports3.wast
$ owi wasm script abstract --no-exhaustion reference/imports4.wast
$ owi wasm script abstract --no-exhaustion reference/linking0.wast
$ owi wasm script abstract --no-exhaustion reference/linking1.wast
$ owi wasm script abstract --no-exhaustion reference/linking2.wast
$ owi wasm script abstract --no-exhaustion reference/linking3.wast
$ owi wasm script abstract --no-exhaustion reference/load.wast
$ owi wasm script abstract --no-exhaustion reference/load0.wast
$ owi wasm script abstract --no-exhaustion reference/load1.wast
$ owi wasm script abstract --no-exhaustion reference/load2.wast
$ owi wasm script abstract --no-exhaustion reference/memory_copy0.wast
$ owi wasm script abstract --no-exhaustion reference/memory_copy1.wast
$ owi wasm script abstract --no-exhaustion reference/memory_fill0.wast
$ owi wasm script abstract --no-exhaustion reference/memory_grow.wast
$ owi wasm script abstract --no-exhaustion reference/memory_init0.wast
$ owi wasm script abstract --no-exhaustion reference/memory_size.wast
$ owi wasm script abstract --no-exhaustion reference/memory_size0.wast
$ owi wasm script abstract --no-exhaustion reference/memory_size1.wast
$ owi wasm script abstract --no-exhaustion reference/memory_size2.wast
$ owi wasm script abstract --no-exhaustion reference/memory_size3.wast
$ owi wasm script abstract --no-exhaustion reference/memory_trap0.wast
$ owi wasm script abstract --no-exhaustion reference/memory_trap1.wast
$ owi wasm script abstract --no-exhaustion reference/memory-multi.wast
$ owi wasm script abstract --no-exhaustion reference/memory.wast
$ owi wasm script abstract --no-exhaustion reference/start0.wast
$ owi wasm script abstract --no-exhaustion reference/store.wast
$ owi wasm script abstract --no-exhaustion reference/store0.wast
$ owi wasm script abstract --no-exhaustion reference/store1.wast
$ owi wasm script abstract --no-exhaustion reference/traps0.wast

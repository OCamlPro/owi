  $ owi script --no-exhaustion --optimize reference/address.wast
  $ owi script --no-exhaustion --optimize reference/align.wast
  $ owi script --no-exhaustion --optimize reference/binary-leb128.wast
  $ owi script --no-exhaustion --optimize reference/binary.wast
  expected END opcode expected but got (unexpected end of section or function)
  [51]
  $ owi script --no-exhaustion --optimize reference/block.wast
  $ owi script --no-exhaustion --optimize reference/br_if.wast
  $ owi script --no-exhaustion --optimize reference/br_table.wast
  $ owi script --no-exhaustion --optimize reference/br.wast
  $ owi script --no-exhaustion --optimize reference/bulk.wast
  $ owi script --no-exhaustion --optimize reference/call_indirect.wast
  $ owi script --no-exhaustion --optimize reference/call.wast
  $ owi script --no-exhaustion --optimize reference/comments.wast
  unexpected token "\"(func (export \"f1\") (result i32)\""
  [40]
  $ owi script --no-exhaustion --optimize reference/const.wast
  $ owi script --no-exhaustion --optimize reference/conversions.wast
  $ owi script --no-exhaustion --optimize reference/custom.wast
  $ owi script --no-exhaustion --optimize reference/data.wast
  $ owi script --no-exhaustion --optimize reference/elem.wast
  $ owi script --no-exhaustion --optimize reference/endianness.wast
  $ owi script --no-exhaustion --optimize reference/exports.wast
  $ owi script --no-exhaustion --optimize reference/f32_bitwise.wast
  $ owi script --no-exhaustion --optimize reference/f32_cmp.wast
  $ owi script --no-exhaustion --optimize reference/f32.wast
  $ owi script --no-exhaustion --optimize reference/f64_bitwise.wast
  $ owi script --no-exhaustion --optimize reference/f64_cmp.wast
  $ owi script --no-exhaustion --optimize reference/f64.wast
  $ owi script --no-exhaustion --optimize reference/fac.wast
  $ owi script --no-exhaustion --optimize reference/float_exprs.wast
  $ owi script --no-exhaustion --optimize reference/float_literals.wast
  unbound name 4294967249
  [38]
  $ owi script --no-exhaustion --optimize reference/float_memory.wast
  $ owi script --no-exhaustion --optimize reference/float_misc.wast
  $ owi script --no-exhaustion --optimize reference/forward.wast
  $ owi script --no-exhaustion --optimize reference/func_ptrs.wast
  83
  $ owi script --no-exhaustion --optimize reference/func.wast
  $ owi script --no-exhaustion --optimize reference/global.wast
  $ owi script --no-exhaustion --optimize reference/i32.wast
  $ owi script --no-exhaustion --optimize reference/i64.wast
  $ owi script --no-exhaustion --optimize reference/if.wast
  $ owi script --no-exhaustion --optimize reference/imports.wast
  13
  14
  42
  13
  13
  13
  13
  24
  25
  53
  24
  24
  24
  24
  13
  got:      f32.const 0
  expected: (f32.const 666.599_975_585_937_5)
  bad result
  [3]
  $ owi script --no-exhaustion --optimize reference/inline-module.wast
  $ owi script --no-exhaustion --optimize reference/int_exprs.wast
  $ owi script --no-exhaustion --optimize reference/int_literals.wast
  $ owi script --no-exhaustion --optimize reference/labels.wast
  $ owi script --no-exhaustion --optimize reference/left-to-right.wast
  $ owi script --no-exhaustion --optimize reference/linking.wast
  $ owi script --no-exhaustion --optimize reference/load.wast
  $ owi script --no-exhaustion --optimize reference/local_get.wast
  $ owi script --no-exhaustion --optimize reference/local_set.wast
  $ owi script --no-exhaustion --optimize reference/local_tee.wast
  $ owi script --no-exhaustion --optimize reference/loop.wast
  $ owi script --no-exhaustion --optimize reference/memory_copy.wast
  $ owi script --no-exhaustion --optimize reference/memory_fill.wast
  $ owi script --no-exhaustion --optimize reference/memory_grow.wast
  $ owi script --no-exhaustion --optimize reference/memory_init.wast
  $ owi script --no-exhaustion --optimize reference/memory_redundancy.wast
  $ owi script --no-exhaustion --optimize reference/memory_size.wast
  $ owi script --no-exhaustion --optimize reference/memory_trap.wast
  $ owi script --no-exhaustion --optimize reference/memory.wast
  $ owi script --no-exhaustion --optimize reference/names.wast
  42
  123
  $ owi script --no-exhaustion --optimize reference/nop.wast
  $ owi script --no-exhaustion --optimize reference/ref_func.wast
  $ owi script --no-exhaustion --optimize reference/ref_is_null.wast
  $ owi script --no-exhaustion --optimize reference/ref_null.wast
  $ owi script --no-exhaustion --optimize reference/proposals/tail-call/return_call_indirect.wast
  $ owi script --no-exhaustion --optimize reference/proposals/tail-call/return_call.wast
  $ owi script --no-exhaustion --optimize reference/return.wast
  $ owi script --no-exhaustion --optimize reference/select.wast
  $ owi script --no-exhaustion --optimize reference/skip-stack-guard-page.wast
  $ owi script --no-exhaustion --optimize reference/stack.wast
  $ owi script --no-exhaustion --optimize reference/start.wast
  1
  2
  $ owi script --no-exhaustion --optimize reference/store.wast
  $ owi script --no-exhaustion --optimize reference/switch.wast
  $ owi script --no-exhaustion --optimize reference/table_copy.wast
  $ owi script --no-exhaustion --optimize reference/table_fill.wast
  $ owi script --no-exhaustion --optimize reference/table_get.wast
  $ owi script --no-exhaustion --optimize reference/table_grow.wast
  $ owi script --no-exhaustion --optimize reference/table_init.wast
  $ owi script --no-exhaustion --optimize reference/table_set.wast
  $ owi script --no-exhaustion --optimize reference/table_size.wast
  $ owi script --no-exhaustion --optimize reference/table-sub.wast
  $ owi script --no-exhaustion --optimize reference/table.wast
  $ owi script --no-exhaustion --optimize reference/token.wast
  $ owi script --no-exhaustion --optimize reference/token.wast
  $ owi script --no-exhaustion --optimize reference/traps.wast
  $ owi script --no-exhaustion --optimize reference/type.wast
  $ owi script --no-exhaustion --optimize reference/unreachable.wast
  $ owi script --no-exhaustion --optimize reference/unreached-invalid.wast
  $ owi script --no-exhaustion --optimize reference/unreached-valid.wast
  $ owi script --no-exhaustion --optimize reference/unwind.wast
  $ owi script --no-exhaustion --optimize reference/utf8-custom-section-id.wast
  $ owi script --no-exhaustion --optimize reference/utf8-import-field.wast
  $ owi script --no-exhaustion --optimize reference/utf8-import-module.wast
  $ owi script --no-exhaustion --optimize reference/utf8-invalid-encoding.wast

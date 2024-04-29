  $ owi script --no-exhaustion reference/address.wast
  $ owi script --no-exhaustion reference/align.wast
  $ owi script --no-exhaustion reference/binary-leb128.wast
  expected integer representation too long but got (unexpected end)
  [51]
  $ owi script --no-exhaustion reference/binary.wast
  expected integer too large but there was no error
  [7]
  $ owi script --no-exhaustion reference/block.wast
  $ owi script --no-exhaustion reference/br_if.wast
  $ owi script --no-exhaustion reference/br_table.wast
  $ owi script --no-exhaustion reference/br.wast
  $ owi script --no-exhaustion reference/bulk.wast
  $ owi script --no-exhaustion reference/call_indirect.wast
  $ owi script --no-exhaustion reference/call.wast
  $ owi script --no-exhaustion reference/comments.wast
  $ owi script --no-exhaustion reference/const.wast
  $ owi script --no-exhaustion reference/conversions.wast
  $ owi script --no-exhaustion reference/custom.wast
  expected function and code section have inconsistent lengths but got (malformed section id)
  [51]
  $ owi script --no-exhaustion reference/data.wast
  expected unknown memory 0 but there was no error
  [7]
  $ owi script --no-exhaustion reference/elem.wast
  $ owi script --no-exhaustion reference/endianness.wast
  $ owi script --no-exhaustion reference/exports.wast
  $ owi script --no-exhaustion reference/f32_bitwise.wast
  $ owi script --no-exhaustion reference/f32_cmp.wast
  $ owi script --no-exhaustion reference/f32.wast
  $ owi script --no-exhaustion reference/f64_bitwise.wast
  $ owi script --no-exhaustion reference/f64_cmp.wast
  $ owi script --no-exhaustion reference/f64.wast
  $ owi script --no-exhaustion reference/fac.wast
  $ owi script --no-exhaustion reference/float_exprs.wast
  $ owi script --no-exhaustion reference/float_literals.wast
  $ owi script --no-exhaustion reference/float_memory.wast
  $ owi script --no-exhaustion reference/float_misc.wast
  $ owi script --no-exhaustion reference/forward.wast
  $ owi script --no-exhaustion reference/func_ptrs.wast
  83
  $ owi script --no-exhaustion reference/func.wast
  $ owi script --no-exhaustion reference/global.wast
  $ owi script --no-exhaustion reference/i32.wast
  $ owi script --no-exhaustion reference/i64.wast
  $ owi script --no-exhaustion reference/if.wast
  $ owi script --no-exhaustion reference/imports.wast
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
  $ owi script --no-exhaustion reference/inline-module.wast
  $ owi script --no-exhaustion reference/int_exprs.wast
  $ owi script --no-exhaustion reference/int_literals.wast
  $ owi script --no-exhaustion reference/labels.wast
  $ owi script --no-exhaustion reference/left-to-right.wast
  $ owi script --no-exhaustion reference/linking.wast
  $ owi script --no-exhaustion reference/load.wast
  $ owi script --no-exhaustion reference/local_get.wast
  $ owi script --no-exhaustion reference/local_set.wast
  $ owi script --no-exhaustion reference/local_tee.wast
  $ owi script --no-exhaustion reference/loop.wast
  $ owi script --no-exhaustion reference/memory_copy.wast
  $ owi script --no-exhaustion reference/memory_fill.wast
  $ owi script --no-exhaustion reference/memory_grow.wast
  $ owi script --no-exhaustion reference/memory_init.wast
  $ owi script --no-exhaustion reference/memory_redundancy.wast
  $ owi script --no-exhaustion reference/memory_size.wast
  $ owi script --no-exhaustion reference/memory_trap.wast
  $ owi script --no-exhaustion reference/memory.wast
  $ owi script --no-exhaustion reference/names.wast
  42
  123
  $ owi script --no-exhaustion reference/nop.wast
  $ owi script --no-exhaustion reference/ref_func.wast
  $ owi script --no-exhaustion reference/ref_is_null.wast
  $ owi script --no-exhaustion reference/ref_null.wast
  $ owi script --no-exhaustion reference/return_call_indirect.wast
  $ owi script --no-exhaustion reference/return_call.wast
  $ owi script --no-exhaustion reference/return.wast
  $ owi script --no-exhaustion reference/select.wast
  $ owi script --no-exhaustion reference/skip-stack-guard-page.wast
  $ owi script --no-exhaustion reference/stack.wast
  $ owi script --no-exhaustion reference/start.wast
  1
  2
  $ owi script --no-exhaustion reference/store.wast
  $ owi script --no-exhaustion reference/switch.wast
  $ owi script --no-exhaustion reference/table_copy.wast
  $ owi script --no-exhaustion reference/table_fill.wast
  $ owi script --no-exhaustion reference/table_get.wast
  $ owi script --no-exhaustion reference/table_grow.wast
  $ owi script --no-exhaustion reference/table_init.wast
  $ owi script --no-exhaustion reference/table_set.wast
  $ owi script --no-exhaustion reference/table_size.wast
  $ owi script --no-exhaustion reference/table-sub.wast
  $ owi script --no-exhaustion reference/table.wast
  $ owi script --no-exhaustion reference/tokens.wast
  $ owi script --no-exhaustion reference/token.wast
  $ owi script --no-exhaustion reference/traps.wast
  $ owi script --no-exhaustion reference/type.wast
  $ owi script --no-exhaustion reference/unreachable.wast
  $ owi script --no-exhaustion reference/unreached-invalid.wast
  $ owi script --no-exhaustion reference/unreached-valid.wast
  $ owi script --no-exhaustion reference/unwind.wast
  $ owi script --no-exhaustion reference/utf8-custom-section-id.wast
  expected malformed UTF-8 encoding but there was no error
  [7]
  $ owi script --no-exhaustion reference/utf8-import-field.wast
  expected malformed UTF-8 encoding but there was no error
  [7]
  $ owi script --no-exhaustion reference/utf8-import-module.wast
  expected malformed UTF-8 encoding but there was no error
  [7]
  $ owi script --no-exhaustion reference/utf8-invalid-encoding.wast

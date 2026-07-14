  $ owi script --no-exhaustion reference/address.wast
  $ owi script --no-exhaustion reference/align.wast
  $ owi script --no-exhaustion reference/binary-leb128.wast
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
  $ owi script --no-exhaustion reference/inline-module.wast
  $ owi script --no-exhaustion reference/int_exprs.wast
  $ owi script --no-exhaustion reference/int_literals.wast
  $ owi script --no-exhaustion reference/labels.wast
  $ owi script --no-exhaustion reference/left-to-right.wast
  $ owi script --no-exhaustion reference/linking.wast
  $ owi script --no-exhaustion reference/local_get.wast
  $ owi script --no-exhaustion reference/local_set.wast
  $ owi script --no-exhaustion reference/local_tee.wast
  $ owi script --no-exhaustion reference/loop.wast
  $ owi script --no-exhaustion reference/memory_copy.wast
  $ owi script --no-exhaustion reference/memory_fill.wast
  $ owi script --no-exhaustion reference/memory_init.wast
  $ owi script --no-exhaustion reference/memory_redundancy.wast
  $ owi script --no-exhaustion reference/memory_trap.wast
  $ owi script --no-exhaustion reference/names.wast
  42
  123
  $ owi script --no-exhaustion reference/nop.wast
  $ owi script --no-exhaustion reference/ref_as_non_null.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented `call_ref`")
  $ owi script --no-exhaustion reference/ref.wast
  owi: [ERROR] expected unknown type but there was no error
  [7]
  $ owi script --no-exhaustion reference/ref_func.wast
  $ owi script --no-exhaustion reference/ref_is_null.wast
  $ owi script --no-exhaustion reference/ref_null.wast
  $ owi script --no-exhaustion reference/return_call_indirect.wast
  5
  91
  $ owi script --no-exhaustion reference/return_call.wast
  5
  91
  $ owi script --no-exhaustion reference/return.wast
  $ owi script --no-exhaustion reference/select.wast
  $ owi script --no-exhaustion reference/skip-stack-guard-page.wast
  $ owi script --no-exhaustion reference/simd_address.wast
  $ owi script --no-exhaustion reference/simd_align.wast
  $ owi script --no-exhaustion reference/simd_bit_shift.wast
  $ owi script --no-exhaustion reference/simd_bitwise.wast
  $ owi script --no-exhaustion reference/simd_boolean.wast
  $ owi script --no-exhaustion reference/simd_const.wast
  $ owi script --no-exhaustion reference/simd_conversions.wast
  $ owi script --no-exhaustion reference/simd_f32x4_arith.wast
  $ owi script --no-exhaustion reference/simd_f32x4_cmp.wast
  $ owi script --no-exhaustion reference/simd_f32x4_pmin_pmax.wast
  $ owi script --no-exhaustion reference/simd_f32x4_rounding.wast
  $ owi script --no-exhaustion reference/simd_f32x4.wast
  $ owi script --no-exhaustion reference/simd_f64x2_arith.wast
  $ owi script --no-exhaustion reference/simd_f64x2_cmp.wast
  $ owi script --no-exhaustion reference/simd_f64x2_pmin_pmax.wast
  $ owi script --no-exhaustion reference/simd_f64x2_rounding.wast
  $ owi script --no-exhaustion reference/simd_f64x2.wast
  $ owi script --no-exhaustion reference/simd_i16x8_arith2.wast
  $ owi script --no-exhaustion reference/simd_i16x8_arith.wast
  $ owi script --no-exhaustion reference/simd_i16x8_cmp.wast
  $ owi script --no-exhaustion reference/simd_i16x8_extadd_pairwise_i8x16.wast
  $ owi script --no-exhaustion reference/simd_i16x8_extmul_i8x16.wast
  $ owi script --no-exhaustion reference/simd_i16x8_q15mulr_sat_s.wast
  $ owi script --no-exhaustion reference/simd_i16x8_sat_arith.wast
  $ owi script --no-exhaustion reference/simd_i32x4_arith2.wast
  $ owi script --no-exhaustion reference/simd_i32x4_arith.wast
  $ owi script --no-exhaustion reference/simd_i32x4_cmp.wast
  $ owi script --no-exhaustion reference/simd_i32x4_dot_i16x8.wast
  $ owi script --no-exhaustion reference/simd_i32x4_extadd_pairwise_i16x8.wast
  $ owi script --no-exhaustion reference/simd_i32x4_extmul_i16x8.wast
  $ owi script --no-exhaustion reference/simd_i32x4_trunc_sat_f32x4.wast
  $ owi script --no-exhaustion reference/simd_i32x4_trunc_sat_f64x2.wast
  $ owi script --no-exhaustion reference/simd_i64x2_arith2.wast
  $ owi script --no-exhaustion reference/simd_i64x2_arith.wast
  $ owi script --no-exhaustion reference/simd_i64x2_cmp.wast
  $ owi script --no-exhaustion reference/simd_i64x2_extmul_i32x4.wast
  $ owi script --no-exhaustion reference/simd_i8x16_arith2.wast
  $ owi script --no-exhaustion reference/simd_i8x16_arith.wast
  $ owi script --no-exhaustion reference/simd_i8x16_cmp.wast
  $ owi script --no-exhaustion reference/simd_i8x16_sat_arith.wast
  $ owi script --no-exhaustion reference/simd_int_to_int_extend.wast
  $ owi script --no-exhaustion reference/simd_lane.wast
  $ owi script --no-exhaustion reference/simd_linking.wast
  $ owi script --no-exhaustion reference/simd_load16_lane.wast
  $ owi script --no-exhaustion reference/simd_load32_lane.wast
  $ owi script --no-exhaustion reference/simd_load64_lane.wast
  $ owi script --no-exhaustion reference/simd_load8_lane.wast
  $ owi script --no-exhaustion reference/simd_load_extend.wast
  $ owi script --no-exhaustion reference/simd_load_splat.wast
  $ owi script --no-exhaustion reference/simd_load.wast
  $ owi script --no-exhaustion reference/simd_load_zero.wast
  $ owi script --no-exhaustion reference/simd_memory-multi.wast
  $ owi script --no-exhaustion reference/simd_select.wast
  $ owi script --no-exhaustion reference/simd_splat.wast
  $ owi script --no-exhaustion reference/simd_store16_lane.wast
  $ owi script --no-exhaustion reference/simd_store32_lane.wast
  $ owi script --no-exhaustion reference/simd_store64_lane.wast
  $ owi script --no-exhaustion reference/simd_store8_lane.wast
  $ owi script --no-exhaustion reference/simd_store.wast
  $ owi script --no-exhaustion reference/start.wast
  1
  2
  $ owi script --no-exhaustion reference/switch.wast
  $ owi script --no-exhaustion reference/table_copy.wast
  $ owi script --no-exhaustion reference/table_fill.wast
  $ owi script --no-exhaustion reference/table_get.wast
  $ owi script --no-exhaustion reference/table_grow.wast
  $ owi script --no-exhaustion reference/table_init.wast 2>&1 | grep -oE "Failure.*"
  Failure("TODO: unimplemented ref instruction interpretation: ref.eq")
  $ owi script --no-exhaustion reference/table_set.wast
  $ owi script --no-exhaustion reference/table_size.wast
  $ owi script --no-exhaustion reference/table-sub.wast
  $ owi script --no-exhaustion reference/table.wast
  $ owi script --no-exhaustion reference/token.wast
  $ owi script --no-exhaustion reference/token.wast
  $ owi script --no-exhaustion reference/traps.wast
  $ owi script --no-exhaustion reference/type.wast
  $ owi script --no-exhaustion reference/unreachable.wast
  $ owi script --no-exhaustion reference/unreached-invalid.wast
  $ owi script --no-exhaustion reference/unreached-valid.wast
  $ owi script --no-exhaustion reference/unwind.wast
  $ owi script --no-exhaustion reference/utf8-custom-section-id.wast
  $ owi script --no-exhaustion reference/utf8-import-field.wast
  $ owi script --no-exhaustion reference/utf8-import-module.wast
  $ owi script --no-exhaustion reference/utf8-invalid-encoding.wast
  $ owi script --no-exhaustion reference/annotations.wast
  owi: [ERROR] unknown operator "\"(@x \\\"\") \"unclosed"
  [23]
  $ owi script --no-exhaustion reference/address0.wast
  $ owi script --no-exhaustion reference/address1.wast
  $ owi script --no-exhaustion reference/align0.wast
  $ owi script --no-exhaustion reference/binary.wast
  $ owi script --no-exhaustion reference/binary0.wast
  $ owi script --no-exhaustion reference/data_drop0.wast
  $ owi script --no-exhaustion reference/data.wast
  $ owi script --no-exhaustion reference/data0.wast
  $ owi script --no-exhaustion reference/data1.wast
  $ owi script --no-exhaustion reference/exports0.wast
  $ owi script --no-exhaustion reference/float_exprs0.wast
  $ owi script --no-exhaustion reference/float_exprs1.wast
  $ owi script --no-exhaustion reference/float_memory0.wast
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
  $ owi script --no-exhaustion reference/imports0.wast
  $ owi script --no-exhaustion reference/imports1.wast
  $ owi script --no-exhaustion reference/imports2.wast
  $ owi script --no-exhaustion reference/imports3.wast
  $ owi script --no-exhaustion reference/imports4.wast
  $ owi script --no-exhaustion reference/linking0.wast
  $ owi script --no-exhaustion reference/linking1.wast
  $ owi script --no-exhaustion reference/linking2.wast
  $ owi script --no-exhaustion reference/linking3.wast
  $ owi script --no-exhaustion reference/load.wast
  $ owi script --no-exhaustion reference/load0.wast
  $ owi script --no-exhaustion reference/load1.wast
  $ owi script --no-exhaustion reference/load2.wast
  $ owi script --no-exhaustion reference/memory_copy0.wast
  $ owi script --no-exhaustion reference/memory_copy1.wast
  $ owi script --no-exhaustion reference/memory_fill0.wast
  $ owi script --no-exhaustion reference/memory_grow.wast
  $ owi script --no-exhaustion reference/memory_init0.wast
  $ owi script --no-exhaustion reference/memory_size.wast
  $ owi script --no-exhaustion reference/memory_size0.wast
  $ owi script --no-exhaustion reference/memory_size1.wast
  $ owi script --no-exhaustion reference/memory_size2.wast
  $ owi script --no-exhaustion reference/memory_size3.wast
  $ owi script --no-exhaustion reference/memory_trap0.wast
  $ owi script --no-exhaustion reference/memory_trap1.wast
  $ owi script --no-exhaustion reference/memory-multi.wast
  $ owi script --no-exhaustion reference/memory.wast
  $ owi script --no-exhaustion reference/start0.wast
  $ owi script --no-exhaustion reference/store.wast
  $ owi script --no-exhaustion reference/store0.wast
  $ owi script --no-exhaustion reference/store1.wast
  $ owi script --no-exhaustion reference/traps0.wast

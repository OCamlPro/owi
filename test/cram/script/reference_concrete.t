  $ owi wasm script concrete --no-exhaustion reference/address.wast
  $ owi wasm script concrete --no-exhaustion reference/align.wast
  $ owi wasm script concrete --no-exhaustion reference/binary-leb128.wast
  $ owi wasm script concrete --no-exhaustion reference/block.wast
  $ owi wasm script concrete --no-exhaustion reference/br_if.wast
  $ owi wasm script concrete --no-exhaustion reference/br_table.wast
  $ owi wasm script concrete --no-exhaustion reference/br.wast
  $ owi wasm script concrete --no-exhaustion reference/bulk.wast
  $ owi wasm script concrete --no-exhaustion reference/call_indirect.wast
  $ owi wasm script concrete --no-exhaustion reference/call.wast
  $ owi wasm script concrete --no-exhaustion reference/comments.wast
  $ owi wasm script concrete --no-exhaustion reference/const.wast
  $ owi wasm script concrete --no-exhaustion reference/conversions.wast
  $ owi wasm script concrete --no-exhaustion reference/custom.wast
  $ owi wasm script concrete --no-exhaustion reference/elem.wast
  $ owi wasm script concrete --no-exhaustion reference/endianness.wast
  $ owi wasm script concrete --no-exhaustion reference/exports.wast
  $ owi wasm script concrete --no-exhaustion reference/f32_bitwise.wast
  $ owi wasm script concrete --no-exhaustion reference/f32_cmp.wast
  $ owi wasm script concrete --no-exhaustion reference/f32.wast
  $ owi wasm script concrete --no-exhaustion reference/f64_bitwise.wast
  $ owi wasm script concrete --no-exhaustion reference/f64_cmp.wast
  $ owi wasm script concrete --no-exhaustion reference/f64.wast
  $ owi wasm script concrete --no-exhaustion reference/fac.wast
  $ owi wasm script concrete --no-exhaustion reference/float_exprs.wast
  $ owi wasm script concrete --no-exhaustion reference/float_literals.wast
  $ owi wasm script concrete --no-exhaustion reference/float_memory.wast
  $ owi wasm script concrete --no-exhaustion reference/float_misc.wast
  $ owi wasm script concrete --no-exhaustion reference/forward.wast
  $ owi wasm script concrete --no-exhaustion reference/func_ptrs.wast
  83
  $ owi wasm script concrete --no-exhaustion reference/func.wast
  $ owi wasm script concrete --no-exhaustion reference/global.wast
  $ owi wasm script concrete --no-exhaustion reference/i32.wast
  $ owi wasm script concrete --no-exhaustion reference/i64.wast
  $ owi wasm script concrete --no-exhaustion reference/if.wast
  $ owi wasm script concrete --no-exhaustion reference/inline-module.wast
  $ owi wasm script concrete --no-exhaustion reference/int_exprs.wast
  $ owi wasm script concrete --no-exhaustion reference/int_literals.wast
  $ owi wasm script concrete --no-exhaustion reference/labels.wast
  $ owi wasm script concrete --no-exhaustion reference/left-to-right.wast
  $ owi wasm script concrete --no-exhaustion reference/linking.wast
  owi: [ERROR] got:      i32.const 6
  expected: (i32.const 5)
  owi: [ERROR] bad result
  [3]
  $ owi wasm script concrete --no-exhaustion reference/local_get.wast
  $ owi wasm script concrete --no-exhaustion reference/local_set.wast
  $ owi wasm script concrete --no-exhaustion reference/local_tee.wast
  $ owi wasm script concrete --no-exhaustion reference/loop.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_copy.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_fill.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_init.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_redundancy.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_trap.wast
  $ owi wasm script concrete --no-exhaustion reference/names.wast
  42
  123
  $ owi wasm script concrete --no-exhaustion reference/nop.wast
  $ owi wasm script concrete --no-exhaustion reference/ref_as_non_null.wast
  $ owi wasm script concrete --no-exhaustion reference/ref.wast
  owi: [ERROR] expected unknown type but there was no error
  [7]
  $ owi wasm script concrete --no-exhaustion reference/ref_func.wast
  $ owi wasm script concrete --no-exhaustion reference/ref_is_null.wast
  $ owi wasm script concrete --no-exhaustion reference/ref_null.wast
  $ owi wasm script concrete --no-exhaustion reference/return_call_indirect.wast
  5
  91
  $ owi wasm script concrete --no-exhaustion reference/return_call.wast
  5
  91
  $ owi wasm script concrete --no-exhaustion reference/return.wast
  $ owi wasm script concrete --no-exhaustion reference/select.wast
  $ owi wasm script concrete --no-exhaustion reference/skip-stack-guard-page.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_address.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_align.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_bit_shift.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_bitwise.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_boolean.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_const.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_conversions.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f32x4_arith.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f32x4_cmp.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f32x4_pmin_pmax.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f32x4_rounding.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f32x4.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f64x2_arith.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f64x2_cmp.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f64x2_pmin_pmax.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f64x2_rounding.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_f64x2.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i16x8_arith2.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i16x8_arith.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i16x8_cmp.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i16x8_extadd_pairwise_i8x16.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i16x8_extmul_i8x16.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i16x8_q15mulr_sat_s.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i16x8_sat_arith.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i32x4_arith2.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i32x4_arith.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i32x4_cmp.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i32x4_dot_i16x8.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i32x4_extadd_pairwise_i16x8.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i32x4_extmul_i16x8.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i32x4_trunc_sat_f32x4.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i32x4_trunc_sat_f64x2.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i64x2_arith2.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i64x2_arith.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i64x2_cmp.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i64x2_extmul_i32x4.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i8x16_arith2.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i8x16_arith.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i8x16_cmp.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_i8x16_sat_arith.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_int_to_int_extend.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_linking.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_load16_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_load32_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_load64_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_load8_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_load_extend.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_load_splat.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_load.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_load_zero.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_memory-multi.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_select.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_splat.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_store16_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_store32_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_store64_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_store8_lane.wast
  $ owi wasm script concrete --no-exhaustion reference/simd_store.wast
  $ owi wasm script concrete --no-exhaustion reference/start.wast
  1
  2
  $ owi wasm script concrete --no-exhaustion reference/switch.wast
  $ owi wasm script concrete --no-exhaustion reference/table_copy.wast
  $ owi wasm script concrete --no-exhaustion reference/table_fill.wast
  $ owi wasm script concrete --no-exhaustion reference/table_get.wast
  $ owi wasm script concrete --no-exhaustion reference/table_grow.wast
  $ owi wasm script concrete --no-exhaustion reference/table_init.wast
  $ owi wasm script concrete --no-exhaustion reference/table_set.wast
  $ owi wasm script concrete --no-exhaustion reference/table_size.wast
  $ owi wasm script concrete --no-exhaustion reference/table-sub.wast
  $ owi wasm script concrete --no-exhaustion reference/table.wast
  $ owi wasm script concrete --no-exhaustion reference/token.wast
  $ owi wasm script concrete --no-exhaustion reference/token.wast
  $ owi wasm script concrete --no-exhaustion reference/traps.wast
  $ owi wasm script concrete --no-exhaustion reference/type.wast
  $ owi wasm script concrete --no-exhaustion reference/unreachable.wast
  $ owi wasm script concrete --no-exhaustion reference/unreached-invalid.wast
  $ owi wasm script concrete --no-exhaustion reference/unreached-valid.wast
  $ owi wasm script concrete --no-exhaustion reference/unwind.wast
  $ owi wasm script concrete --no-exhaustion reference/utf8-custom-section-id.wast
  $ owi wasm script concrete --no-exhaustion reference/utf8-import-field.wast
  $ owi wasm script concrete --no-exhaustion reference/utf8-import-module.wast
  $ owi wasm script concrete --no-exhaustion reference/utf8-invalid-encoding.wast
  $ owi wasm script concrete --no-exhaustion reference/annotations.wast
  owi: [ERROR] unknown operator "\"(@x \\\"\") \"unclosed"
  [23]
  $ owi wasm script concrete --no-exhaustion reference/address0.wast
  $ owi wasm script concrete --no-exhaustion reference/address1.wast
  $ owi wasm script concrete --no-exhaustion reference/align0.wast
  $ owi wasm script concrete --no-exhaustion reference/binary.wast
  $ owi wasm script concrete --no-exhaustion reference/binary0.wast
  $ owi wasm script concrete --no-exhaustion reference/data_drop0.wast
  $ owi wasm script concrete --no-exhaustion reference/data.wast
  $ owi wasm script concrete --no-exhaustion reference/data0.wast
  $ owi wasm script concrete --no-exhaustion reference/data1.wast
  $ owi wasm script concrete --no-exhaustion reference/exports0.wast
  $ owi wasm script concrete --no-exhaustion reference/float_exprs0.wast
  $ owi wasm script concrete --no-exhaustion reference/float_exprs1.wast
  $ owi wasm script concrete --no-exhaustion reference/float_memory0.wast
  $ owi wasm script concrete --no-exhaustion reference/imports.wast
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
  $ owi wasm script concrete --no-exhaustion reference/imports0.wast
  $ owi wasm script concrete --no-exhaustion reference/imports1.wast
  $ owi wasm script concrete --no-exhaustion reference/imports2.wast
  $ owi wasm script concrete --no-exhaustion reference/imports3.wast
  $ owi wasm script concrete --no-exhaustion reference/imports4.wast
  $ owi wasm script concrete --no-exhaustion reference/linking0.wast
  owi: [ERROR] uninitialized element 7
  [94]
  $ owi wasm script concrete --no-exhaustion reference/linking1.wast
  owi: [ERROR] got:      i32.const 167
  expected: (i32.const 242)
  owi: [ERROR] bad result
  [3]
  $ owi wasm script concrete --no-exhaustion reference/linking2.wast
  $ owi wasm script concrete --no-exhaustion reference/linking3.wast
  $ owi wasm script concrete --no-exhaustion reference/load.wast
  $ owi wasm script concrete --no-exhaustion reference/load0.wast
  $ owi wasm script concrete --no-exhaustion reference/load1.wast
  $ owi wasm script concrete --no-exhaustion reference/load2.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_copy0.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_copy1.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_fill0.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_grow.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_init0.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_size.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_size0.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_size1.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_size2.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_size3.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_trap0.wast
  $ owi wasm script concrete --no-exhaustion reference/memory_trap1.wast
  $ owi wasm script concrete --no-exhaustion reference/memory-multi.wast
  $ owi wasm script concrete --no-exhaustion reference/memory.wast
  $ owi wasm script concrete --no-exhaustion reference/start0.wast
  $ owi wasm script concrete --no-exhaustion reference/store.wast
  $ owi wasm script concrete --no-exhaustion reference/store0.wast
  $ owi wasm script concrete --no-exhaustion reference/store1.wast
  $ owi wasm script concrete --no-exhaustion reference/traps0.wast

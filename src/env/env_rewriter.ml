(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module IntMap = Map.Make (Int)

type t =
  { globals : int IntMap.t
  ; memories : int IntMap.t
  ; elems : int IntMap.t
  ; datas : int IntMap.t
  ; tables : int IntMap.t
  ; functions : int IntMap.t
  ; tags : int IntMap.t
  ; type_base_id : int
  }

let empty =
  let globals = IntMap.empty in
  let memories = IntMap.empty in
  let elems = IntMap.empty in
  let datas = IntMap.empty in
  let tables = IntMap.empty in
  let functions = IntMap.empty in
  let tags = IntMap.empty in
  let type_base_id = 0 in
  { globals; memories; elems; datas; tables; functions; type_base_id; tags }

(* Now this is the second step, where we rewrite all access to use env address.
                                 For instance, if a function contains the instruction global.get 0, the 0 is local to the modul in which the function is defined.
                                 We look what is the env address of this global in the map, by looking the global map at (module_id, 0).
                                 If the env address is say, 42, we rewrite the instruction to be global.get 42. *)
let get_unsafe k tbl =
  match IntMap.find_opt k tbl with Some v -> v | None -> assert false

let rewrite_global_instruction ~map : Binary.global_instr -> Binary.global_instr
    = function
  | Get i -> Get (get_unsafe i map.globals)
  | Set i -> Set (get_unsafe i map.globals)

let rewrite_i32_instruction ~map = function
  | (( Const _ | Clz | Ctz | Popcnt | Add | Sub | Mul | Div_s | Div_u | Rem_s
     | Rem_u | And | Or | Xor | Shl | Shr_s | Shr_u | Rotl | Rotr | Eqz | Eq
     | Ne | Lt_s | Lt_u | Gt_s | Gt_u | Le_s | Le_u | Ge_s | Ge_u | Extend8_s
     | Extend16_s | Wrap_i64 | Trunc_f_s _ | Trunc_f_u _ | Trunc_sat_f_s _
     | Trunc_sat_f_u _ | Reinterpret_f _ ) :
      Binary.i32_instr ) as i ->
    i
  | Load (i, memarg) -> Load (get_unsafe i map.memories, memarg)
  | Load8_s (i, memarg) -> Load8_s (get_unsafe i map.memories, memarg)
  | Load8_u (i, memarg) -> Load8_u (get_unsafe i map.memories, memarg)
  | Load16_s (i, memarg) -> Load16_s (get_unsafe i map.memories, memarg)
  | Load16_u (i, memarg) -> Load16_u (get_unsafe i map.memories, memarg)
  | Store (i, memarg) -> Store (get_unsafe i map.memories, memarg)
  | Store8 (i, memarg) -> Store8 (get_unsafe i map.memories, memarg)
  | Store16 (i, memarg) -> Store16 (get_unsafe i map.memories, memarg)

let rewrite_i64_instruction ~map = function
  | (( Const _ | Clz | Ctz | Popcnt | Add | Sub | Mul | Div_s | Div_u | Rem_s
     | Rem_u | And | Or | Xor | Shl | Shr_s | Shr_u | Rotl | Rotr | Eqz | Eq
     | Ne | Lt_s | Lt_u | Gt_s | Gt_u | Le_s | Le_u | Ge_s | Ge_u | Extend8_s
     | Extend16_s | Trunc_f_s _ | Trunc_f_u _ | Trunc_sat_f_s _
     | Trunc_sat_f_u _ | Reinterpret_f _ | Extend32_s | Extend_i32_s
     | Extend_i32_u ) :
      Binary.i64_instr ) as i ->
    i
  | Load (i, memarg) -> Load (get_unsafe i map.memories, memarg)
  | Load8_s (i, memarg) -> Load8_s (get_unsafe i map.memories, memarg)
  | Load8_u (i, memarg) -> Load8_u (get_unsafe i map.memories, memarg)
  | Load16_s (i, memarg) -> Load16_s (get_unsafe i map.memories, memarg)
  | Load16_u (i, memarg) -> Load16_u (get_unsafe i map.memories, memarg)
  | Load32_s (i, memarg) -> Load32_s (get_unsafe i map.memories, memarg)
  | Load32_u (i, memarg) -> Load32_u (get_unsafe i map.memories, memarg)
  | Store (i, memarg) -> Store (get_unsafe i map.memories, memarg)
  | Store8 (i, memarg) -> Store8 (get_unsafe i map.memories, memarg)
  | Store16 (i, memarg) -> Store16 (get_unsafe i map.memories, memarg)
  | Store32 (i, memarg) -> Store32 (get_unsafe i map.memories, memarg)

let rewrite_f32_instruction ~map = function
  | (( Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest | Sub | Mul | Div | Min
     | Max | Copysign | Eq | Ne | Lt | Gt | Le | Ge | Demote_f64 | Const _
     | Convert_i_s _ | Convert_i_u _ | Reinterpret_i _ | Add ) :
      Binary.f32_instr ) as i ->
    i
  | Load (i, memarg) -> Load (get_unsafe i map.memories, memarg)
  | Store (i, memarg) -> Store (get_unsafe i map.memories, memarg)

let rewrite_f64_instruction ~map = function
  | (( Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest | Add | Sub | Mul | Div
     | Min | Max | Copysign | Eq | Ne | Lt | Gt | Le | Ge | Promote_f32
     | Const _ | Convert_i_s _ | Convert_i_u _ | Reinterpret_i _ ) :
      Binary.f64_instr ) as i ->
    i
  | Load (i, memarg) -> Load (get_unsafe i map.memories, memarg)
  | Store (i, memarg) -> Store (get_unsafe i map.memories, memarg)

let rewrite_v128_instruction ~map = function
  | (And | Not | Or | Any_true | Bitselect | Xor | Andnot | Const _ :
      Binary.v128_instr ) as i ->
    i
  | Load8_splat (i, memarg) -> Load8_splat (get_unsafe i map.memories, memarg)
  | Load8_lane (i, memarg, n) ->
    Load8_lane (get_unsafe i map.memories, memarg, n)
  | Load8x8_s (i, memarg) -> Load8x8_s (get_unsafe i map.memories, memarg)
  | Load8x8_u (i, memarg) -> Load8x8_u (get_unsafe i map.memories, memarg)
  | Load16_splat (i, memarg) -> Load16_splat (get_unsafe i map.memories, memarg)
  | Load16_lane (i, memarg, n) ->
    Load16_lane (get_unsafe i map.memories, memarg, n)
  | Load16x4_s (i, memarg) -> Load16x4_s (get_unsafe i map.memories, memarg)
  | Load16x4_u (i, memarg) -> Load16x4_u (get_unsafe i map.memories, memarg)
  | Load32_splat (i, memarg) -> Load32_splat (get_unsafe i map.memories, memarg)
  | Load32_lane (i, memarg, n) ->
    Load32_lane (get_unsafe i map.memories, memarg, n)
  | Load32_zero (i, memarg) -> Load32_zero (get_unsafe i map.memories, memarg)
  | Load64_splat (i, memarg) -> Load64_splat (get_unsafe i map.memories, memarg)
  | Load64_lane (i, memarg, n) ->
    Load64_lane (get_unsafe i map.memories, memarg, n)
  | Load64_zero (i, memarg) -> Load64_zero (get_unsafe i map.memories, memarg)
  | Load (i, memarg) -> Load (get_unsafe i map.memories, memarg)
  | Store (i, memarg) -> Store (get_unsafe i map.memories, memarg)
  | Store8_lane (i, memarg, n) ->
    Store8_lane (get_unsafe i map.memories, memarg, n)
  | Store64_lane (i, memarg, n) ->
    Store64_lane (get_unsafe i map.memories, memarg, n)
  | Store32_zero (i, memarg) -> Store32_zero (get_unsafe i map.memories, memarg)
  | Store32_lane (i, memarg, n) ->
    Store32_lane (get_unsafe i map.memories, memarg, n)
  | Store16_lane (i, memarg, n) ->
    Store16_lane (get_unsafe i map.memories, memarg, n)
  | Load32x2_s (i, memarg) -> Load32x2_s (get_unsafe i map.memories, memarg)
  | Load32x2_u (i, memarg) -> Load32x2_u (get_unsafe i map.memories, memarg)

let rewrite_i8x16_instruction : Text.i8x16_instr -> Text.i8x16_instr = function
  | ( Add | Sub | Eq | Ne | Lt_s | Lt_u | Gt_s | Gt_u | Le_s | Le_u | Ge_s
    | Ge_u | Abs | Neg | Popcnt | All_true | Bitmask | Swizzle | Splat | Shl
    | Shr_s | Shr_u | Min_s | Min_u | Add_sat_s | Add_sat_u | Sub_sat_s
    | Sub_sat_u | Max_s | Max_u | Narrow_i16x8_s | Narrow_i16x8_u | Avgr_u
    | Shuffle _ | Extract_lane_s _ | Extract_lane_u _ | Replace_lane _ ) as i ->
    i

let rewrite_i16x8_instruction : Text.i16x8_instr -> Text.i16x8_instr = function
  | ( Add | Sub | Mul | Eq | Ne | Lt_s | Lt_u | Gt_s | Gt_u | Le_s | Le_u | Ge_s
    | Ge_u | Splat | Q15mulr_sat_s | Min_s | Min_u | Extmul_low_i8x16_s
    | Extmul_low_i8x16_u | Extmul_high_i8x16_s | Extmul_high_i8x16_u
    | Extend_low_i8x16_s | Extend_low_i8x16_u | Extend_high_i8x16_s
    | Extend_high_i8x16_u | Extadd_pairwise_i8x16_s | Extadd_pairwise_i8x16_u
    | Add_sat_s | Add_sat_u | Sub_sat_s | Sub_sat_u | Max_s | Max_u | Shl | Neg
    | All_true | Shr_s | Shr_u | Bitmask | Avgr_u | Abs | Narrow_i32x4_s
    | Narrow_i32x4_u | Extract_lane_s _ | Extract_lane_u _ | Replace_lane _ ) as
    i ->
    i

let rewrite_i32x4_instruction : Text.i32x4_instr -> Text.i32x4_instr = function
  | ( Add | Sub | Mul | Shl | Shr_s | Shr_u | Eq | Ne | Lt_s | Lt_u | Gt_s
    | Gt_u | Le_s | Le_u | Ge_s | Ge_u | Splat | Extend_low_i16x8_s
    | Extend_high_i16x8_s | Extend_low_i16x8_u | Extend_high_i16x8_u
    | Trunc_sat_f64x2_s_zero | Trunc_sat_f64x2_u_zero | Trunc_sat_f32x4_s
    | Trunc_sat_f32x4_u | Min_s | Min_u | Extmul_low_i16x8_s
    | Extmul_low_i16x8_u | Extmul_high_i16x8_s | Extmul_high_i16x8_u
    | Extadd_pairwise_i16x8_s | Extadd_pairwise_i16x8_u | Dot_i16x8_s | Neg
    | Max_s | Max_u | Abs | All_true | Bitmask | Extract_lane _ | Replace_lane _
      ) as i ->
    i

let rewrite_i64x2_instruction : Text.i64x2_instr -> Text.i64x2_instr = function
  | ( Add | Sub | Mul | Eq | Ne | Lt_s | Gt_s | Le_s | Ge_s | Splat
    | Extend_low_i32x4_s | Extend_low_i32x4_u | Extend_high_i32x4_s
    | Extend_high_i32x4_u | Extmul_low_i32x4_s | Extmul_low_i32x4_u
    | Extmul_high_i32x4_s | Extmul_high_i32x4_u | Abs | Neg | All_true | Bitmask
    | Shl | Shr_s | Shr_u | Extract_lane _ | Replace_lane _ ) as i ->
    i

let rewrite_f32x4_instruction : Text.f32x4_instr -> Text.f32x4_instr = function
  | ( Add | Pmin | Min | Eq | Convert_i32x4_s | Convert_i32x4_u | Ceil | Max
    | Floor | Pmax | Ne | Sub | Abs | Trunc | Lt | Gt | Le | Ge | Mul
    | Convert_low_i32x4_s | Convert_low_i32x4_u | Convert_high_i32x4_s
    | Convert_high_i32x4_u | Splat | Nearest | Div | Neg | Sqrt
    | Demote_f64x2_zero | Extract_lane _ | Replace_lane _ ) as i ->
    i

let rewrite_f64x2_instruction : Text.f64x2_instr -> Text.f64x2_instr = function
  | ( Add | Pmin | Min | Eq | Ceil | Max | Floor | Pmax | Ne | Sub | Abs | Trunc
    | Lt | Gt | Le | Ge | Mul | Convert_low_i32x4_s | Convert_low_i32x4_u
    | Convert_high_i32x4_s | Convert_high_i32x4_u | Nearest | Div | Neg | Sqrt
    | Splat | Promote_low_f32x4 | Extract_lane _ | Replace_lane _ ) as i ->
    i

let rewrite_type_id ~map id = map.type_base_id + id

let rewrite_heap_type ~map : Binary.heap_type -> Binary.heap_type = function
  | TypeUse id -> TypeUse (rewrite_type_id ~map id)
  | ht -> ht

let rewrite_ref_type ~map : Binary.ref_type -> Binary.ref_type =
 fun (nullable, ht) -> (nullable, rewrite_heap_type ~map ht)

let rewrite_val_type ~map : Binary.val_type -> Binary.val_type = function
  | Ref_type rt -> Ref_type (rewrite_ref_type ~map rt)
  | vt -> vt

let rewrite_storage_type ~map : Binary.storage_type -> Binary.storage_type =
  function
  | Val_type vt -> Val_type (rewrite_val_type ~map vt)
  | Pack_type _ as pt -> pt

let rewrite_field_type ~map : Binary.field_type -> Binary.field_type =
 fun (mut, st) -> (mut, rewrite_storage_type ~map st)

let rewrite_comp_type ~map : Binary.comp_type -> Binary.comp_type = function
  | Def_struct_t fields ->
    Def_struct_t
      (List.map (fun (id, ft) -> (id, rewrite_field_type ~map ft)) fields)
  | Def_array_t ft -> Def_array_t (rewrite_field_type ~map ft)
  | Def_func_t (params, results) ->
    Def_func_t
      ( List.map (fun (id, vt) -> (id, rewrite_val_type ~map vt)) params
      , List.map (rewrite_val_type ~map) results )

let rewrite_sub_type ~map : Binary.sub_type -> Binary.sub_type =
 fun { final; ids; ct } ->
  let ids = List.map (rewrite_type_id ~map) ids in
  let ct = rewrite_comp_type ~map ct in
  { final; ct; ids }

let rewrite_ref_instruction ~map = function
  | (Null ht : Binary.ref_instr) -> Binary.Null (rewrite_heap_type ~map ht)
  | Test rt -> Test (rewrite_ref_type ~map rt)
  | Cast rt -> Cast (rewrite_ref_type ~map rt)
  | (Is_null | As_non_null | Eq) as i -> i
  | Func i -> Func (get_unsafe i map.functions)

let rewrite_table_instruction ~map : Binary.table_instr -> Binary.table_instr =
  function
  | Get i -> Get (get_unsafe i map.tables)
  | Set i -> Set (get_unsafe i map.tables)
  | Size i -> Size (get_unsafe i map.tables)
  | Grow i -> Grow (get_unsafe i map.tables)
  | Fill i -> Fill (get_unsafe i map.tables)
  | Copy (i1, i2) -> Copy (get_unsafe i1 map.tables, get_unsafe i2 map.tables)
  | Init (i1, i2) -> Init (get_unsafe i1 map.tables, get_unsafe i2 map.elems)

let rewrite_elem_instruction ~map : Binary.elem_instr -> Binary.elem_instr =
  function
  | Drop i -> Drop (get_unsafe i map.elems)

let rewrite_memory_instruction ~map : Binary.memory_instr -> Binary.memory_instr
    = function
  | Size i -> Size (get_unsafe i map.memories)
  | Grow i -> Grow (get_unsafe i map.memories)
  | Fill i -> Fill (get_unsafe i map.memories)
  | Copy (i1, i2) ->
    Copy (get_unsafe i1 map.memories, get_unsafe i2 map.memories)
  | Init (i1, i2) -> Init (get_unsafe i1 map.memories, get_unsafe i2 map.datas)

let rewrite_data_instruction ~map : Binary.data_instr -> Binary.data_instr =
  function
  | Drop i -> Drop (get_unsafe i map.datas)

let rewrite_struct_instruction ~map : Binary.struct_instr -> Binary.struct_instr
    = function
  | New id -> New (rewrite_type_id ~map id)
  | New_default id -> New_default (rewrite_type_id ~map id)
  | Get (ty, fld) -> Get (rewrite_type_id ~map ty, fld)
  | Get_s (ty, fld) -> Get_s (rewrite_type_id ~map ty, fld)
  | Get_u (ty, fld) -> Get_u (rewrite_type_id ~map ty, fld)
  | Set (ty, fld) -> Set (rewrite_type_id ~map ty, fld)

let rewrite_array_instruction ~map : Binary.array_instr -> Binary.array_instr =
  function
  | New id -> New (rewrite_type_id ~map id)
  | New_default id -> New_default (rewrite_type_id ~map id)
  | New_fixed (id, n) -> New_fixed (rewrite_type_id ~map id, n)
  | New_data (ty, data) ->
    New_data (rewrite_type_id ~map ty, get_unsafe data map.datas)
  | New_elem (ty, elem) ->
    New_elem (rewrite_type_id ~map ty, get_unsafe elem map.elems)
  | Get id -> Get (rewrite_type_id ~map id)
  | Get_s id -> Get_s (rewrite_type_id ~map id)
  | Get_u id -> Get_u (rewrite_type_id ~map id)
  | Set id -> Set (rewrite_type_id ~map id)
  | Fill id -> Fill (rewrite_type_id ~map id)
  | Copy (id1, id2) -> Copy (rewrite_type_id ~map id1, rewrite_type_id ~map id2)
  | Init_data (ty, data) ->
    Init_data (rewrite_type_id ~map ty, get_unsafe data map.datas)
  | Init_elem (ty, elem) ->
    Init_elem (rewrite_type_id ~map ty, get_unsafe elem map.elems)
  | Len as i -> i

let rewrite_simple_instruction ~map = function
  | (Global i : Binary.simple_instruction) ->
    Binary.Global (rewrite_global_instruction ~map i)
  | I32 i -> I32 (rewrite_i32_instruction ~map i)
  | I64 i -> I64 (rewrite_i64_instruction ~map i)
  | F32 i -> F32 (rewrite_f32_instruction ~map i)
  | F64 i -> F64 (rewrite_f64_instruction ~map i)
  | V128 i -> V128 (rewrite_v128_instruction ~map i)
  | I8x16 i -> I8x16 (rewrite_i8x16_instruction i)
  | I16x8 i -> I16x8 (rewrite_i16x8_instruction i)
  | I32x4 i -> I32x4 (rewrite_i32x4_instruction i)
  | I64x2 i -> I64x2 (rewrite_i64x2_instruction i)
  | F32x4 i -> F32x4 (rewrite_f32x4_instruction i)
  | F64x2 i -> F64x2 (rewrite_f64x2_instruction i)
  | Ref i -> Ref (rewrite_ref_instruction ~map i)
  | Table i -> Table (rewrite_table_instruction ~map i)
  | Elem i -> Elem (rewrite_elem_instruction ~map i)
  | Memory i -> Memory (rewrite_memory_instruction ~map i)
  | Data i -> Data (rewrite_data_instruction ~map i)
  | ( Nop | Local _ | Drop | Unreachable | Any_convert_extern
    | Extern_convert_any | Select _ | I31 _ ) as i ->
    i
  | Struct i -> Struct (rewrite_struct_instruction ~map i)
  | Array i -> Array (rewrite_array_instruction ~map i)

let rewrite_block_type ~map : Binary.block_type -> Binary.block_type =
 fun (type_id_opt, ft) ->
  ( Option.map (rewrite_type_id ~map) type_id_opt
  , ( List.map (fun (id, vt) -> (id, rewrite_val_type ~map vt)) (fst ft)
    , List.map (rewrite_val_type ~map) (snd ft) ) )

let rec rewrite_instruction ~map = function
  | Binary.Simple i -> Binary.Simple (rewrite_simple_instruction ~map i)
  | Block (a, b, e) ->
    Block (a, Option.map (rewrite_block_type ~map) b, rewrite_expression ~map e)
  | Loop (a, b, e) ->
    Loop (a, Option.map (rewrite_block_type ~map) b, rewrite_expression ~map e)
  | If_else (a, b, e1, e2) ->
    If_else
      ( a
      , Option.map (rewrite_block_type ~map) b
      , rewrite_expression e1 ~map
      , rewrite_expression e2 ~map )
  | Return_call i -> Return_call (get_unsafe i map.functions)
  | Call i -> Call (get_unsafe i map.functions)
  | Call_indirect (i, typ) ->
    Call_indirect (get_unsafe i map.tables, rewrite_block_type ~map typ)
  | Return_call_indirect (i, typ) ->
    Return_call_indirect (get_unsafe i map.tables, rewrite_block_type ~map typ)
  | Br_on_cast (id, rt1, rt2) ->
    Br_on_cast (id, rewrite_ref_type ~map rt1, rewrite_ref_type ~map rt2)
  | Br_on_cast_fail (id, rt1, rt2) ->
    Br_on_cast_fail (id, rewrite_ref_type ~map rt1, rewrite_ref_type ~map rt2)
  | ( Return | Br _ | Br_if _ | Br_table _ | Br_on_null _ | Br_on_non_null _
    (* TODO: It's weird that return_call_ref is not using an indice like call_ref does... *)
    | Return_call_ref _
    (* TODO: check that call_ref is taking a raw type and not a typed index *)
    | Call_ref _ ) as i ->
    i

and rewrite_expression ~map expr =
  Annotated.map (List.map (Annotated.map (rewrite_instruction ~map))) expr

let rewrite_binary_func ~map (func : Binary.Func.t) : _ Kind.func =
  let body = rewrite_expression ~map func.body in
  let type_f = rewrite_block_type ~map func.type_f in
  Kind.Wasm { func with body; type_f }

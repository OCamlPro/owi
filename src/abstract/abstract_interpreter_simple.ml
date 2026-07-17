(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Abstract_stack

type t =
  | State of Abstract_state.t
  | Unreachable

let i32_can_be_zero ctx v =
  match Abstract_domain.query_boolean ctx (Abstract_i32.eqz ctx v) with
  | True | Top -> true
  | False | Bottom -> false

let eval_i32 ({ stack; ctx; invariant; _ } as state : Abstract_state.t) uuid :
  Binary.i32_instr -> _ = function
  | Const i ->
    let stack = Stack.push_i32 stack (Abstract_i32.of_int32 ctx i) in
    State { state with stack }
  | Add ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.add ctx) in
    State { state with stack }
  | Sub ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.sub ctx) in
    State { state with stack }
  | Mul ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.mul ctx) in
    State { state with stack }
  | Div_s ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.div_s ctx hd1 hd2) in
    State { state with stack }
  | Div_u ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.div_u ctx hd1 hd2) in
    State { state with stack }
  | Rem_s ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.rem_s ctx hd1 hd2) in
    State { state with stack }
  | Rem_u ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.rem_u ctx hd1 hd2) in
    State { state with stack }
  | And ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.and_ ctx) in
    State { state with stack }
  | Or ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.or_ ctx) in
    State { state with stack }
  | Xor ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.xor ctx) in
    State { state with stack }
  | Shl ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.shl ctx) in
    State { state with stack }
  | Lt_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_s ctx) in
    State { state with stack }
  | Gt_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.gt_s ctx) in
    State { state with stack }
  | Lt_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_u ctx) in
    State { state with stack }
  | Gt_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.gt_u ctx) in
    State { state with stack }
  | Le_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_s ctx) in
    State { state with stack }
  | Ge_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ge_s ctx) in
    State { state with stack }
  | Le_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_u ctx) in
    State { state with stack }
  | Ge_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ge_u ctx) in
    State { state with stack }
  | Ne ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ne ctx) in
    State { state with stack }
  | Eqz ->
    let stack = Stack.apply_i32_boolean stack ctx (Abstract_i32.eqz ctx) in
    State { state with stack }
  | Eq ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.eq ctx) in
    State { state with stack }
  | Store (_memid, _) | Store8 (_memid, _) | Store16 (_memid, _) ->
    (* TODO: handle this correctly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    State { state with stack }
  | Load (_memid, _)
  | Load8_s (_memid, _)
  | Load8_u (_memid, _)
  | Load16_s (_memid, _)
  | Load16_u (_memid, _) ->
    (* TODO: handle it correctly *)
    let _, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    State { state with stack }
  | Clz | Ctz | Popcnt ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    State { state with stack }
  | Shr_s | Shr_u ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    State { state with stack }
  | Rotl | Rotr ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    State { state with stack }
  | Extend8_s ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.extend_s ctx 8) in
    State { state with stack }
  | Extend16_s ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.extend_s ctx 16) in
    State { state with stack }
  | Wrap_i64 ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.wrap_i64 ctx) in
    State { state with stack }
  | Trunc_f_s _nn | Trunc_f_u _nn | Trunc_sat_f_s _nn | Trunc_sat_f_u _nn ->
    (* TODO: handle correctly *)
    let _f, stack = Stack.pop_f32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    State { state with stack }
  | Reinterpret_f _nn ->
    (* TODO: handle nn *)
    let f, stack = Stack.pop_f32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.of_binary f) in
    State { state with stack }

let i64_can_be_zero ctx v =
  match Abstract_domain.query_boolean ctx (Abstract_i64.eqz ctx v) with
  | Top | True -> true
  | Bottom | False -> false

let eval_i64 ({ stack; ctx; invariant; _ } as state : Abstract_state.t) uuid :
  Binary.i64_instr -> _ = function
  | Const i ->
    let stack = Stack.push_i64 stack (Abstract_i64.of_int64 ctx i) in
    State { state with stack }
  | Add ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.add ctx) in
    State { state with stack }
  | Sub ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.sub ctx) in
    State { state with stack }
  | Mul ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.mul ctx) in
    State { state with stack }
  | Div_s ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.div_s ctx hd1 hd2) in
    State { state with stack }
  | Div_u ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.div_u ctx hd1 hd2) in
    State { state with stack }
  | Rem_s ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.rem_s ctx hd1 hd2) in
    State { state with stack }
  | Rem_u ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.rem_u ctx hd1 hd2) in
    State { state with stack }
  | And ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.and_ ctx) in
    State { state with stack }
  | Or ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.or_ ctx) in
    State { state with stack }
  | Xor ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.xor ctx) in
    State { state with stack }
  | Shl ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.shl ctx) in
    State { state with stack }
  | Lt_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_s ctx) in
    State { state with stack }
  | Gt_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.gt_s ctx) in
    State { state with stack }
  | Lt_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_u ctx) in
    State { state with stack }
  | Gt_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.gt_u ctx) in
    State { state with stack }
  | Le_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_s ctx) in
    State { state with stack }
  | Ge_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ge_s ctx) in
    State { state with stack }
  | Le_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_u ctx) in
    State { state with stack }
  | Ge_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ge_u ctx) in
    State { state with stack }
  | Ne ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ne ctx) in
    State { state with stack }
  | Eqz ->
    let stack = Stack.apply_i64_boolean stack ctx (Abstract_i64.eqz ctx) in
    State { state with stack }
  | Eq ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.eq ctx) in
    State { state with stack }
  | Extend8_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 8) in
    State { state with stack }
  | Extend16_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 16) in
    State { state with stack }
  | Extend32_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 32) in
    State { state with stack }
  | Extend_i32_s ->
    let stack = Stack.apply_i32_i64 stack (Abstract_i64.extend_i32_s ctx) in
    State { state with stack }
  | Extend_i32_u ->
    let stack = Stack.apply_i32_i64 stack (Abstract_i64.extend_i32_u ctx) in
    State { state with stack }
  | Store (_memid, _)
  | Store8 (_memid, _)
  | Store16 (_memid, _)
  | Store32 (_memid, _) ->
    (* TODO: handle this correctly *)
    let (_n, _pos), stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    State { state with stack }
  | Load (_memid, _)
  | Load8_s (_memid, _)
  | Load8_u (_memid, _)
  | Load16_s (_memid, _)
  | Load16_u (_memid, _)
  | Load32_s (_memid, _)
  | Load32_u (_memid, _) ->
    (* TODO: handle this correctly *)
    let _pos, stack = Stack.pop_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    State { state with stack }
  | Reinterpret_f _nn ->
    (* TODO: handle nn *)
    let f, stack = Stack.pop_f64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.of_binary f) in
    State { state with stack }
  | Clz | Ctz | Popcnt ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    State { state with stack }
  | Shr_s | Shr_u ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    State { state with stack }
  | Rotl | Rotr ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    State { state with stack }
  | Trunc_f_s _nn | Trunc_f_u _nn | Trunc_sat_f_s _nn | Trunc_sat_f_u _nn ->
    (* TODO: handle correctly *)
    let _f, stack = Stack.pop_f32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    State { state with stack }

(* TODO: handle this correctly *)
let eval_f32 ({ stack; ctx; _ } as state : Abstract_state.t) _uuid :
  Binary.f32_instr -> _ = function
  | Const f ->
    let stack = Stack.push_f32 stack (Abstract_f32.of_float32 ctx f) in
    State { state with stack }
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    let stack =
      Stack.apply_f32_f32_f32 stack (fun _ _ -> Abstract_f32.unknown ctx)
    in
    State { state with stack }
  | Lt | Le | Gt | Ge | Eq | Ne ->
    let stack =
      Stack.apply_f32_f32_boolean stack ctx (fun _ _ ->
        Abstract_boolean.unknown ctx )
    in
    State { state with stack }
  | Convert_i_s nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx)
      | S64 -> Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx)
    in
    State { state with stack }
  | Convert_i_u nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx)
      | S64 -> Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx)
    in
    State { state with stack }
  | Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt ->
    let stack = Stack.apply_f32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    State { state with stack }
  | Demote_f64 ->
    let stack = Stack.apply_f64_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    State { state with stack }
  | Reinterpret_i S32 ->
    let stack = Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    State { state with stack }
  | Reinterpret_i S64 ->
    let stack = Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    State { state with stack }
  | Load (_i, _m) ->
    let stack = Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    State { state with stack }
  | Store (_i, _m) ->
    let _, stack = Stack.pop_f32 stack in
    let _, stack = Stack.pop_i32 stack in
    State { state with stack }

(* TODO: handle this correctly *)
let eval_f64 ({ stack; ctx; _ } as state : Abstract_state.t) _uuid :
  Binary.f64_instr -> _ = function
  | Const _ ->
    let stack = Stack.push_f64 stack (Abstract_f64.unknown ctx) in
    State { state with stack }
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    let stack =
      Stack.apply_f64_f64_f64 stack (fun _ _ -> Abstract_f64.unknown ctx)
    in
    State { state with stack }
  | Lt | Le | Gt | Ge | Eq | Ne ->
    let stack =
      Stack.apply_f64_f64_boolean stack ctx (fun _ _ ->
        Abstract_boolean.unknown ctx )
    in
    State { state with stack }
  | Convert_i_s nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx)
      | S64 -> Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx)
    in
    State { state with stack }
  | Convert_i_u nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx)
      | S64 -> Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx)
    in
    State { state with stack }
  | Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt ->
    let stack = Stack.apply_f64_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    State { state with stack }
  | Promote_f32 ->
    let stack =
      Stack.apply_f32_f64 stack (fun f -> Abstract_f64.of_float ctx f)
    in
    State { state with stack }
  | Reinterpret_i S32 ->
    let stack = Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    State { state with stack }
  | Reinterpret_i S64 ->
    let stack = Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    State { state with stack }
  | Load (_i, _m) ->
    let stack = Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    State { state with stack }
  | Store (_i, _m) ->
    let _, stack = Stack.pop_f64 stack in
    let _, stack = Stack.pop_i32 stack in
    State { state with stack }

let eval_local ({ stack; locals; _ } as state : Abstract_state.t) :
  Binary.local_instr -> _ = function
  | Get i ->
    let v = Abstract_locals.find i locals in
    let stack = Stack.push stack v in
    State { state with stack }
  | Set i ->
    let e, stack = Stack.pop stack in
    let locals = Abstract_locals.add i e locals in
    State { state with stack; locals }
  | Tee i ->
    let e, stack = Stack.pop stack in
    let stack = Stack.push stack e in
    let locals = Abstract_locals.add i e locals in
    State { state with stack; locals }

let eval_global ({ stack; globals; _ } as state : Abstract_state.t) :
  Binary.global_instr -> _ = function
  | Set i ->
    let e, stack = Stack.pop stack in
    let globals = Abstract_globals.add i e globals in
    State { state with stack; globals }
  | Get i ->
    let v = Abstract_globals.find i globals in
    let stack = Stack.push stack v in
    State { state with stack; globals }

(* TODO: handle this correctly *)
let eval_memory (state : Abstract_state.t) : Binary.memory_instr -> _ = function
  | Size _i | Grow _i | Fill _i -> State state
  | Init (_i1, _i2) | Copy (_i1, _i2) -> State state

(* TODO: handle this correctly *)
let eval_data (state : Abstract_state.t) : Binary.data_instr -> _ = function
  | Drop _i -> State state

let eval_instr ({ stack; _ } as state : Abstract_state.t) :
  Binary.instr Annotated.t -> t =
 fun { raw; uuid; _ } ->
  match raw with
  | I32 instr -> eval_i32 state uuid instr
  | I64 instr -> eval_i64 state uuid instr
  | F32 instr -> eval_f32 state uuid instr
  | F64 instr -> eval_f64 state uuid instr
  | Unreachable -> Unreachable
  | Local instr -> eval_local state instr
  | Global instr -> eval_global state instr
  | Memory instr -> eval_memory state instr
  | Data instr -> eval_data state instr
  | Nop -> State state
  | Drop ->
    let _, stack = Stack.pop stack in
    State { state with stack }
  | If_else _ | Call _ | Call_indirect _ | Call_ref _ | Return | Return_call _
  | Return_call_indirect _ | Return_call_ref _ | Block _ | Loop _ | Br _
  | Br_if _ | Br_table _ | Br_on_non_null _ | Br_on_null _ | Select _
  | Br_on_cast (_, _, _)
  | Br_on_cast_fail (_, _, _) ->
    Log.err (fun m -> m "Control flow instruction given to simple interpreter");
    assert false
  | V128 _ | I8x16 _ | I16x8 _ | I32x4 _ | I64x2 _ | F32x4 _ | F64x2 _ ->
    Fmt.failwith "no SIMD support yet"
  | ( Ref _ | Table _ | Elem _ | I31 _ | Struct _ | Array _ | Any_convert_extern
    | Extern_convert_any ) as instr ->
    Fmt.failwith "%a not implemented in simple interpreter"
      (Binary.pp_instr ~short:true)
      instr

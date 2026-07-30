(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Abstract_stack

type t =
  | State of Abstract_interpreter_state.t
  | Unreachable

let i32_can_be_zero ctx v =
  match Abstract_domain.query_boolean ctx (Abstract_i32.eqz ctx v) with
  | True | Top -> true
  | False | Bottom -> false

let eval_i32 runtime
  ({ stack; ctx; invariant; _ } as abs_state : Abstract_state.t) uuid :
  Binary.i32_instr -> _ = function
  | Const i ->
    let stack = Stack.push_i32 stack (Abstract_i32.of_int32 ctx i) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.add ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.sub ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Mul ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.mul ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Div_s ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.div_s ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Div_u ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.div_u ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Rem_s ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.rem_s ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Rem_u ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.rem_u ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | And ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.and_ ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Or ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.or_ ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Xor ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.xor ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shl ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.shl ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.gt_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.gt_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ge_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ge_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ne ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ne ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eqz ->
    let stack = Stack.apply_i32_boolean stack ctx (Abstract_i32.eqz ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.eq ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store (_memid, _) | Store8 (_memid, _) | Store16 (_memid, _) ->
    (* TODO: handle this correctly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load (_memid, _)
  | Load8_s (_memid, _)
  | Load8_u (_memid, _)
  | Load16_s (_memid, _)
  | Load16_u (_memid, _) ->
    (* TODO: handle it correctly *)
    let _, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Clz | Ctz | Popcnt ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_s | Shr_u ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Rotl | Rotr ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend8_s ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.extend_s ctx 8) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend16_s ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.extend_s ctx 16) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Wrap_i64 ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.wrap_i64 ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Trunc_f_s nn | Trunc_f_u nn | Trunc_sat_f_s nn | Trunc_sat_f_u nn ->
    (* TODO: handle correctly *)
    let _f, stack =
      match nn with
      | Text.S32 -> Stack.pop_f32 stack
      | Text.S64 -> Stack.pop_f64 stack
    in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Reinterpret_f _nn ->
    (* TODO: handle nn *)
    let f, stack = Stack.pop_f32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.of_binary f) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

let i64_can_be_zero ctx v =
  match Abstract_domain.query_boolean ctx (Abstract_i64.eqz ctx v) with
  | Top | True -> true
  | Bottom | False -> false

let eval_i64 runtime
  ({ stack; ctx; invariant; _ } as abs_state : Abstract_state.t) uuid :
  Binary.i64_instr -> _ = function
  | Const i ->
    let stack = Stack.push_i64 stack (Abstract_i64.of_int64 ctx i) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.add ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.sub ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Mul ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.mul ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Div_s ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.div_s ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Div_u ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.div_u ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Rem_s ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.rem_s ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Rem_u ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.rem_u ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | And ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.and_ ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Or ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.or_ ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Xor ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.xor ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shl ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.shl ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.gt_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.gt_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ge_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ge_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ne ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ne ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eqz ->
    let stack = Stack.apply_i64_boolean stack ctx (Abstract_i64.eqz ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.eq ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend8_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 8) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend16_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 16) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend32_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 32) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_i32_s ->
    let stack = Stack.apply_i32_i64 stack (Abstract_i64.extend_i32_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_i32_u ->
    let stack = Stack.apply_i32_i64 stack (Abstract_i64.extend_i32_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store (_memid, _)
  | Store8 (_memid, _)
  | Store16 (_memid, _)
  | Store32 (_memid, _) ->
    (* TODO: handle this correctly *)
    let (_n, _pos), stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
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
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Reinterpret_f Text.S32 ->
    let f, stack = Stack.pop_f32 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.of_binary f) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Reinterpret_f Text.S64 ->
    let f, stack = Stack.pop_f64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.of_binary f) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Clz | Ctz | Popcnt ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_s | Shr_u ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Rotl | Rotr ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Trunc_f_s nn | Trunc_f_u nn | Trunc_sat_f_s nn | Trunc_sat_f_u nn ->
    (* TODO: handle correctly *)
    let _f, stack =
      match nn with
      | Text.S32 -> Stack.pop_f32 stack
      | Text.S64 -> Stack.pop_f64 stack
    in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_f32 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) _uuid :
  Binary.f32_instr -> _ = function
  | Const f ->
    let stack = Stack.push_f32 stack (Abstract_f32.of_float32 ctx f) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    let stack =
      Stack.apply_f32_f32_f32 stack (fun _ _ -> Abstract_f32.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt | Le | Gt | Ge | Eq | Ne ->
    let stack =
      Stack.apply_f32_f32_boolean stack ctx (fun _ _ ->
        Abstract_boolean.unknown ctx )
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_i_s nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx)
      | S64 -> Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_i_u nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx)
      | S64 -> Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt ->
    let stack = Stack.apply_f32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Demote_f64 ->
    let stack = Stack.apply_f64_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Reinterpret_i S32 ->
    let stack = Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Reinterpret_i S64 ->
    let stack = Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load (_i, _m) ->
    let stack = Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store (_i, _m) ->
    let _, stack = Stack.pop_f32 stack in
    let _, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_f64 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) _uuid :
  Binary.f64_instr -> _ = function
  | Const _ ->
    let stack = Stack.push_f64 stack (Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    let stack =
      Stack.apply_f64_f64_f64 stack (fun _ _ -> Abstract_f64.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt | Le | Gt | Ge | Eq | Ne ->
    let stack =
      Stack.apply_f64_f64_boolean stack ctx (fun _ _ ->
        Abstract_boolean.unknown ctx )
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_i_s nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx)
      | S64 -> Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_i_u nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx)
      | S64 -> Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt ->
    let stack = Stack.apply_f64_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Promote_f32 ->
    let stack =
      Stack.apply_f32_f64 stack (fun f -> Abstract_f64.of_float ctx f)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Reinterpret_i S32 ->
    let stack = Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Reinterpret_i S64 ->
    let stack = Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load (_i, _m) ->
    let stack = Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store (_i, _m) ->
    let _, stack = Stack.pop_f64 stack in
    let _, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_v128 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Binary.v128_instr -> _ = function
  | Const _ ->
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Not ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | And ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Andnot ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Or ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Xor ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Any_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_bool stack ctx (Abstract_boolean.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Bitselect ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load32_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load64_zero _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load16x4_s _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load16x4_u _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load8_splat _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load8_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load8x8_s _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load8x8_u _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load16_splat _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load16_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load32_splat _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load32_zero _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load64_splat _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load64_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store8_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store64_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store32_zero _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store32_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Store16_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load32x2_s _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Load32x2_u _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_i8x16 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.i8x16_instr -> _ = function
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Popcnt ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | All_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Bitmask ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Swizzle ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Splat ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shuffle _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shl ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Min_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extract_lane_s _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extract_lane_u _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_s ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_u ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Min_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add_sat_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub_sat_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Max_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Max_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Narrow_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Narrow_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Avgr_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_i16x8 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.i16x8_instr -> _ = function
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Splat ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extract_lane_s _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extract_lane_u _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Q15mulr_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Min_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Min_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_low_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_low_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_high_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_high_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_low_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_low_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_high_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_high_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extadd_pairwise_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extadd_pairwise_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add_sat_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub_sat_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Max_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Max_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shl ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | All_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_s ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_u ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Bitmask ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Avgr_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Narrow_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Narrow_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_i32x4 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.i32x4_instr -> _ = function
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shl ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_s ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_u ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Splat ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extract_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_low_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_high_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_low_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_high_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Trunc_sat_f64x2_s_zero ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Trunc_sat_f64x2_u_zero ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Trunc_sat_f32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Trunc_sat_f32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Min_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Min_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_low_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_low_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_high_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_high_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extadd_pairwise_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extadd_pairwise_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Dot_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Max_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Max_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | All_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Bitmask ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_i64x2 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.i64x2_instr -> _ = function
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_low_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_low_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Splat ->
    let _v, stack = Stack.pop_i64 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_high_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extend_high_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_low_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_low_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_high_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extmul_high_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | All_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Bitmask ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shl ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_s ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Shr_u ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extract_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_i64 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_f32x4 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.f32x4_instr -> _ = function
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Pmin ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Min ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ceil ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Max ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Floor ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Pmax ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Trunc ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_low_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_low_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_high_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_high_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Splat ->
    let _v, stack = Stack.pop_f32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Nearest ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Div ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sqrt ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Demote_f64x2_zero ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extract_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_f32 stack (Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_f32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_f64x2 runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.f64x2_instr -> _ = function
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Pmin ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Min ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ceil ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Max ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Floor ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Pmax ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Trunc ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Lt ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Gt ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Le ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Ge ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_low_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_low_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_high_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Convert_high_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Nearest ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Div ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Sqrt ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Splat ->
    let _v, stack = Stack.pop_f64 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Promote_low_f32x4 ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Extract_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_f64 stack (Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_f64 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

let eval_local runtime ({ stack; locals; _ } as abs_state : Abstract_state.t) :
  Binary.local_instr -> _ = function
  | Get i ->
    let v = Abstract_locals.find i locals in
    let stack = Stack.push stack v in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Set i ->
    let e, stack = Stack.pop stack in
    let locals = Abstract_locals.add i e locals in
    let abs_state = { abs_state with stack; locals } in
    State { abs_state; runtime }
  | Tee i ->
    let e, stack = Stack.pop stack in
    let stack = Stack.push stack e in
    let locals = Abstract_locals.add i e locals in
    let abs_state = { abs_state with stack; locals } in
    State { abs_state; runtime }

let eval_global runtime ({ stack; _ } as abs_state : Abstract_state.t) :
  Binary.global_instr -> _ = function
  | Set i ->
    let e, stack = Stack.pop stack in
    let runtime = Abstract_runtime.set_global ~runtime i e in
    let abs_state = { abs_state with stack } in
    State { runtime; abs_state }
  | Get i ->
    let v = Abstract_runtime.get_global ~runtime i in
    let stack = Stack.push stack v in
    let abs_state = { abs_state with stack } in
    State { runtime; abs_state }

(* TODO: handle this correctly *)
let eval_memory runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Binary.memory_instr -> _ = function
  | Size _i ->
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Grow _i ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Fill _i ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Init (_i1, _i2) ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Copy (_i1, _i2) ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

(* TODO: handle this correctly *)
let eval_data runtime (abs_state : Abstract_state.t) : Binary.data_instr -> _ =
  function
  | Drop _i -> State { runtime; abs_state }

(* TODO: handle this correctly *)
let eval_elem runtime (abs_state : Abstract_state.t) : Binary.elem_instr -> _ =
  function
  | Drop _i -> State { runtime; abs_state }

(* TODO: handle this correctly *)
let eval_ref runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Binary.ref_instr -> _ = function
  | Null _ ->
    let stack = Stack.push_ref stack Abstract_ref.NullRef in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Is_null ->
    let _v, stack = Stack.pop_ref stack in
    let stack = Stack.push_bool stack ctx (Abstract_boolean.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | As_non_null ->
    let _v, stack = Stack.pop_ref stack in
    let stack = Stack.push_ref stack Abstract_ref.NullRef in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Func _ ->
    let stack = Stack.push_ref stack Abstract_ref.NullRef in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Eq | Test _ | Cast _ -> (* TODO *) assert false

(* TODO: handle this correctly *)
let eval_table runtime ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Binary.table_instr -> _ = function
  | Get _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_ref stack Abstract_ref.NullRef in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Set _ ->
    let _v, stack = Stack.pop_ref stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Size _ ->
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Grow _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_ref stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Fill _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_ref stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Copy _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Init _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }

let eval_instr
  (gen_new_value :
       widens:bool
    -> Abstract_stack.Value.t
    -> Abstract_stack.Value.t
    -> Abstract_interpreter_state.t
    -> Abstract_interpreter_state.t
    -> ( Abstract_value.t
       , Abstract_domain.Context.empty_tuple )
       Abstract_domain.Context.result
    -> ('a -> 'b -> 'a)
    -> (Abstract_value.t, 'c) Abstract_domain.Context.result )
  ({ abs_state; runtime } as state : Abstract_interpreter_state.t) ~uuid :
  Binary.simple_instruction -> t = function
  | I32 instr -> eval_i32 runtime abs_state uuid instr
  | I64 instr -> eval_i64 runtime abs_state uuid instr
  | F32 instr -> eval_f32 runtime abs_state uuid instr
  | F64 instr -> eval_f64 runtime abs_state uuid instr
  | V128 instr -> eval_v128 runtime abs_state instr
  | I8x16 instr -> eval_i8x16 runtime abs_state instr
  | I16x8 instr -> eval_i16x8 runtime abs_state instr
  | I32x4 instr -> eval_i32x4 runtime abs_state instr
  | I64x2 instr -> eval_i64x2 runtime abs_state instr
  | F32x4 instr -> eval_f32x4 runtime abs_state instr
  | F64x2 instr -> eval_f64x2 runtime abs_state instr
  | Local instr -> eval_local runtime abs_state instr
  | Global instr -> eval_global runtime abs_state instr
  | Memory instr -> eval_memory runtime abs_state instr
  | Data instr -> eval_data runtime abs_state instr
  | Elem instr -> eval_elem runtime abs_state instr
  | Ref instr -> eval_ref runtime abs_state instr
  | Table instr -> eval_table runtime abs_state instr
  | Unreachable -> Unreachable
  | Nop -> State { runtime; abs_state }
  | Drop ->
    let _, stack = Stack.pop abs_state.stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; runtime }
  | Select _t ->
    let b, stack = Stack.pop_bool abs_state.stack abs_state.ctx in
    let (v1, v2), stack = Stack.pop2 stack in
    begin match Abstract_domain.query_boolean abs_state.ctx b with
    | Top ->
      (* TODO test *)
      let init_res =
        Abstract_domain.Context.Result
          ( true
          , Abstract_domain.Context.empty_tuple ()
          , fun _ctx out -> (v1, out) )
      in
      let (Abstract_domain.Context.Result (_inc, intup, cont)) =
        gen_new_value ~widens:false v1 v2 state state init_res Fun.const
      in
      let out = Abstract_domain.nondet_same_context abs_state.ctx intup in
      let v = fst @@ cont abs_state.ctx out in
      let stack = Stack.push stack v in
      let abs_state = { abs_state with stack } in
      State { abs_state; runtime }
    | True ->
      let stack = Stack.push stack v1 in
      let abs_state = { abs_state with stack } in
      State { abs_state; runtime }
    | False ->
      let stack = Stack.push stack v2 in
      let abs_state = { abs_state with stack } in
      State { abs_state; runtime }
    | Bottom -> Unreachable
    end
  | I31 _ | Struct _ | Array _ | Any_convert_extern | Extern_convert_any ->
    (* TODO *) assert false

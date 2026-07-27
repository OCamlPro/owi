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

let eval_i32 env ({ stack; ctx; invariant; _ } as abs_state : Abstract_state.t)
  uuid : Binary.i32_instr -> _ = function
  | Const i ->
    let stack = Stack.push_i32 stack (Abstract_i32.of_int32 ctx i) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.add ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.sub ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Mul ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.mul ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Div_s ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.div_s ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Div_u ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.div_u ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Rem_s ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.rem_s ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Rem_u ->
    let (hd1, hd2), stack = Stack.pop2_i32 stack in
    let () =
      let possible = i32_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i32 stack (Abstract_i32.rem_u ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | And ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.and_ ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Or ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.or_ ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Xor ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.xor ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shl ->
    let stack = Stack.apply_i32_i32_i32 stack (Abstract_i32.shl ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.gt_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.lt_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.gt_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_s ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ge_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.le_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_u ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ge_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ne ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.ne ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eqz ->
    let stack = Stack.apply_i32_boolean stack ctx (Abstract_i32.eqz ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq ->
    let stack = Stack.apply_i32_i32_boolean stack ctx (Abstract_i32.eq ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store (_memid, _) | Store8 (_memid, _) | Store16 (_memid, _) ->
    (* TODO: handle this correctly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load (_memid, _)
  | Load8_s (_memid, _)
  | Load8_u (_memid, _)
  | Load16_s (_memid, _)
  | Load16_u (_memid, _) ->
    (* TODO: handle it correctly *)
    let _, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Clz | Ctz | Popcnt ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_s | Shr_u ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Rotl | Rotr ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend8_s ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.extend_s ctx 8) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend16_s ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.extend_s ctx 16) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Wrap_i64 ->
    let stack = Stack.apply_i32_i32 stack (Abstract_i32.wrap_i64 ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Trunc_f_s nn | Trunc_f_u nn | Trunc_sat_f_s nn | Trunc_sat_f_u nn ->
    (* TODO: handle correctly *)
    let _f, stack =
      match nn with
      | Text.S32 -> Stack.pop_f32 stack
      | Text.S64 -> Stack.pop_f64 stack
    in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Reinterpret_f _nn ->
    (* TODO: handle nn *)
    let f, stack = Stack.pop_f32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.of_binary f) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

let i64_can_be_zero ctx v =
  match Abstract_domain.query_boolean ctx (Abstract_i64.eqz ctx v) with
  | Top | True -> true
  | Bottom | False -> false

let eval_i64 env ({ stack; ctx; invariant; _ } as abs_state : Abstract_state.t)
  uuid : Binary.i64_instr -> _ = function
  | Const i ->
    let stack = Stack.push_i64 stack (Abstract_i64.of_int64 ctx i) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.add ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.sub ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Mul ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.mul ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Div_s ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.div_s ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Div_u ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.div_u ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Rem_s ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.rem_s ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Rem_u ->
    let (hd1, hd2), stack = Stack.pop2_i64 stack in
    let () =
      let possible = i64_can_be_zero ctx hd2 in
      Abstract_invariant.add_divide_by_zero_invariant invariant ~uuid ~possible
    in
    let stack = Stack.push_i64 stack (Abstract_i64.rem_u ctx hd1 hd2) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | And ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.and_ ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Or ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.or_ ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Xor ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.xor ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shl ->
    let stack = Stack.apply_i64_i64_i64 stack (Abstract_i64.shl ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.gt_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.lt_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.gt_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_s ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ge_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.le_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_u ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ge_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ne ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.ne ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eqz ->
    let stack = Stack.apply_i64_boolean stack ctx (Abstract_i64.eqz ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq ->
    let stack = Stack.apply_i64_i64_boolean stack ctx (Abstract_i64.eq ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend8_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 8) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend16_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 16) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend32_s ->
    let stack = Stack.apply_i64_i64 stack (Abstract_i64.extend_s ctx 32) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_i32_s ->
    let stack = Stack.apply_i32_i64 stack (Abstract_i64.extend_i32_s ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_i32_u ->
    let stack = Stack.apply_i32_i64 stack (Abstract_i64.extend_i32_u ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store (_memid, _)
  | Store8 (_memid, _)
  | Store16 (_memid, _)
  | Store32 (_memid, _) ->
    (* TODO: handle this correctly *)
    let (_n, _pos), stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
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
    State { abs_state; env }
  | Reinterpret_f Text.S32 ->
    let f, stack = Stack.pop_f32 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.of_binary f) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Reinterpret_f Text.S64 ->
    let f, stack = Stack.pop_f64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.of_binary f) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Clz | Ctz | Popcnt ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_s | Shr_u ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Rotl | Rotr ->
    (* TODO: handle it properly *)
    let _, stack = Stack.pop2_i64 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Trunc_f_s nn | Trunc_f_u nn | Trunc_sat_f_s nn | Trunc_sat_f_u nn ->
    (* TODO: handle correctly *)
    let _f, stack =
      match nn with
      | Text.S32 -> Stack.pop_f32 stack
      | Text.S64 -> Stack.pop_f64 stack
    in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_f32 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) _uuid :
  Binary.f32_instr -> _ = function
  | Const f ->
    let stack = Stack.push_f32 stack (Abstract_f32.of_float32 ctx f) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    let stack =
      Stack.apply_f32_f32_f32 stack (fun _ _ -> Abstract_f32.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt | Le | Gt | Ge | Eq | Ne ->
    let stack =
      Stack.apply_f32_f32_boolean stack ctx (fun _ _ ->
        Abstract_boolean.unknown ctx )
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_i_s nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx)
      | S64 -> Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_i_u nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx)
      | S64 -> Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt ->
    let stack = Stack.apply_f32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Demote_f64 ->
    let stack = Stack.apply_f64_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Reinterpret_i S32 ->
    let stack = Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Reinterpret_i S64 ->
    let stack = Stack.apply_i64_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load (_i, _m) ->
    let stack = Stack.apply_i32_f32 stack (fun _ -> Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store (_i, _m) ->
    let _, stack = Stack.pop_f32 stack in
    let _, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_f64 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) _uuid :
  Binary.f64_instr -> _ = function
  | Const _ ->
    let stack = Stack.push_f64 stack (Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add | Sub | Mul | Div | Min | Max | Copysign ->
    let stack =
      Stack.apply_f64_f64_f64 stack (fun _ _ -> Abstract_f64.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt | Le | Gt | Ge | Eq | Ne ->
    let stack =
      Stack.apply_f64_f64_boolean stack ctx (fun _ _ ->
        Abstract_boolean.unknown ctx )
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_i_s nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx)
      | S64 -> Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_i_u nn ->
    let stack =
      match nn with
      | S32 -> Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx)
      | S64 -> Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Abs | Neg | Ceil | Floor | Trunc | Nearest | Sqrt ->
    let stack = Stack.apply_f64_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Promote_f32 ->
    let stack =
      Stack.apply_f32_f64 stack (fun f -> Abstract_f64.of_float ctx f)
    in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Reinterpret_i S32 ->
    let stack = Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Reinterpret_i S64 ->
    let stack = Stack.apply_i64_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load (_i, _m) ->
    let stack = Stack.apply_i32_f64 stack (fun _ -> Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store (_i, _m) ->
    let _, stack = Stack.pop_f64 stack in
    let _, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_v128 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Binary.v128_instr -> _ = function
  | Const _ ->
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Not ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | And ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Andnot ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Or ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Xor ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Any_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_bool stack ctx (Abstract_boolean.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Bitselect ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load32_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load64_zero _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load16x4_s _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load16x4_u _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load8_splat _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load8_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load8x8_s _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load8x8_u _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load16_splat _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load16_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load32_splat _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load32_zero _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load64_splat _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load64_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store8_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store64_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store32_zero _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store32_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Store16_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load32x2_s _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Load32x2_u _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_i8x16 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.i8x16_instr -> _ = function
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Popcnt ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | All_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Bitmask ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Swizzle ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Splat ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shuffle _ ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shl ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Min_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extract_lane_s _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extract_lane_u _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_s ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_u ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Min_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add_sat_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub_sat_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Max_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Max_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Narrow_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Narrow_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Avgr_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_i16x8 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.i16x8_instr -> _ = function
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Splat ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extract_lane_s _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extract_lane_u _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Q15mulr_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Min_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Min_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_low_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_low_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_high_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_high_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_low_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_low_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_high_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_high_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extadd_pairwise_i8x16_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extadd_pairwise_i8x16_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add_sat_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub_sat_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub_sat_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Max_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Max_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shl ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | All_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_s ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_u ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Bitmask ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Avgr_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Narrow_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Narrow_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_i32x4 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.i32x4_instr -> _ = function
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shl ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_s ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_u ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Splat ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extract_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_low_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_high_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_low_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_high_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Trunc_sat_f64x2_s_zero ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Trunc_sat_f64x2_u_zero ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Trunc_sat_f32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Trunc_sat_f32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Min_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Min_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_low_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_low_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_high_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_high_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extadd_pairwise_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extadd_pairwise_i16x8_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Dot_i16x8_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Max_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Max_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | All_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Bitmask ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_i64x2 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.i64x2_instr -> _ = function
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_low_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_low_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Splat ->
    let _v, stack = Stack.pop_i64 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_high_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extend_high_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_low_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_low_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_high_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extmul_high_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | All_true ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Bitmask ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shl ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_s ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Shr_u ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extract_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_i64 stack (Abstract_i64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_i64 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_f32x4 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.f32x4_instr -> _ = function
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Pmin ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Min ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ceil ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Max ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Floor ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Pmax ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Trunc ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_low_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_low_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_high_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_high_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Splat ->
    let _v, stack = Stack.pop_f32 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Nearest ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Div ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sqrt ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Demote_f64x2_zero ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extract_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_f32 stack (Abstract_f32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_f32 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_f64x2 env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Text.f64x2_instr -> _ = function
  | Abs ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Pmin ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Min ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ceil ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Add ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Max ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Floor ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Pmax ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ne ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sub ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Trunc ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Lt ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Gt ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Le ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Ge ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Mul ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_low_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_low_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_high_i32x4_s ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Convert_high_i32x4_u ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Nearest ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Div ->
    let _v, stack = Stack.pop_v128 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Neg ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Sqrt ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Splat ->
    let _v, stack = Stack.pop_f64 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Promote_low_f32x4 ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Extract_lane _ ->
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_f64 stack (Abstract_f64.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Replace_lane _ ->
    let _v, stack = Stack.pop_f64 stack in
    let _v, stack = Stack.pop_v128 stack in
    let stack = Stack.push_v128 stack (Abstract_v128.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

let eval_local env ({ stack; locals; _ } as abs_state : Abstract_state.t) :
  Binary.local_instr -> _ = function
  | Get i ->
    let v = Abstract_locals.find i locals in
    let stack = Stack.push stack v in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Set i ->
    let e, stack = Stack.pop stack in
    let locals = Abstract_locals.add i e locals in
    let abs_state = { abs_state with stack; locals } in
    State { abs_state; env }
  | Tee i ->
    let e, stack = Stack.pop stack in
    let stack = Stack.push stack e in
    let locals = Abstract_locals.add i e locals in
    let abs_state = { abs_state with stack; locals } in
    State { abs_state; env }

let eval_global env ({ stack; _ } as abs_state : Abstract_state.t) :
  Binary.global_instr -> _ = function
  | Set i ->
    let e, stack = Stack.pop stack in
    let env = Env.Abstract.set_global ~env i e in
    let abs_state = { abs_state with stack } in
    State { env; abs_state }
  | Get i ->
    let v = Env.Abstract.get_global ~env i in
    let stack = Stack.push stack v in
    let abs_state = { abs_state with stack } in
    State { env; abs_state }

(* TODO: handle this correctly *)
let eval_memory env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Binary.memory_instr -> _ = function
  | Size _i ->
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Grow _i ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Fill _i ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Init (_i1, _i2) ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Copy (_i1, _i2) ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

(* TODO: handle this correctly *)
let eval_data env (abs_state : Abstract_state.t) : Binary.data_instr -> _ =
  function
  | Drop _i -> State { env; abs_state }

(* TODO: handle this correctly *)
let eval_elem env (abs_state : Abstract_state.t) : Binary.elem_instr -> _ =
  function
  | Drop _i -> State { env; abs_state }

(* TODO: handle this correctly *)
let eval_ref env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Binary.ref_instr -> _ = function
  | Null _ ->
    let stack = Stack.push_ref stack Abstract_ref.NullRef in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Is_null ->
    let _v, stack = Stack.pop_ref stack in
    let stack = Stack.push_bool stack ctx (Abstract_boolean.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | As_non_null ->
    let _v, stack = Stack.pop_ref stack in
    let stack = Stack.push_ref stack Abstract_ref.NullRef in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Func _ ->
    let stack = Stack.push_ref stack Abstract_ref.NullRef in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Eq | Test _ | Cast _ -> (* TODO *) assert false

(* TODO: handle this correctly *)
let eval_table env ({ stack; ctx; _ } as abs_state : Abstract_state.t) :
  Binary.table_instr -> _ = function
  | Get _ ->
    let _v, stack = Stack.pop_i32 stack in
    let stack = Stack.push_ref stack Abstract_ref.NullRef in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Set _ ->
    let _v, stack = Stack.pop_ref stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Size _ ->
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Grow _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_ref stack in
    let stack = Stack.push_i32 stack (Abstract_i32.unknown ctx) in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Fill _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_ref stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Copy _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
  | Init _ ->
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let _v, stack = Stack.pop_i32 stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }

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
  ({ abs_state; env } as state : Abstract_interpreter_state.t) ~uuid :
  Binary.simple_instruction -> t = function
  | I32 instr -> eval_i32 env abs_state uuid instr
  | I64 instr -> eval_i64 env abs_state uuid instr
  | F32 instr -> eval_f32 env abs_state uuid instr
  | F64 instr -> eval_f64 env abs_state uuid instr
  | V128 instr -> eval_v128 env abs_state instr
  | I8x16 instr -> eval_i8x16 env abs_state instr
  | I16x8 instr -> eval_i16x8 env abs_state instr
  | I32x4 instr -> eval_i32x4 env abs_state instr
  | I64x2 instr -> eval_i64x2 env abs_state instr
  | F32x4 instr -> eval_f32x4 env abs_state instr
  | F64x2 instr -> eval_f64x2 env abs_state instr
  | Local instr -> eval_local env abs_state instr
  | Global instr -> eval_global env abs_state instr
  | Memory instr -> eval_memory env abs_state instr
  | Data instr -> eval_data env abs_state instr
  | Elem instr -> eval_elem env abs_state instr
  | Ref instr -> eval_ref env abs_state instr
  | Table instr -> eval_table env abs_state instr
  | Unreachable -> Unreachable
  | Nop -> State { env; abs_state }
  | Drop ->
    let _, stack = Stack.pop abs_state.stack in
    let abs_state = { abs_state with stack } in
    State { abs_state; env }
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
      State { abs_state; env }
    | True ->
      let stack = Stack.push stack v1 in
      let abs_state = { abs_state with stack } in
      State { abs_state; env }
    | False ->
      let stack = Stack.push stack v2 in
      let abs_state = { abs_state with stack } in
      State { abs_state; env }
    | Bottom -> Unreachable
    end
  | I31 _ | Struct _ | Array _ | Any_convert_extern | Extern_convert_any ->
    (* TODO *) assert false

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let check_can_divide_by_zero invariants uuid pp instr =
  if Abstract_invariant.can_divide_by_zero invariants ~uuid then
    Log.warn (fun m ->
      m "Possible division by zero for expression:(uuid: %i) %a" uuid pp instr )
  else
    Log.info (fun m ->
      m "Passed division by zero check for expression:(uuid: %i) %a" uuid pp
        instr )

let check_i32 ~uuid ~invariants : Binary.i32_instr -> unit = function
  | (Div_s | Div_u | Rem_s | Rem_u) as instr ->
    check_can_divide_by_zero invariants uuid Binary.pp_i32_instr instr
  | Const _ | Clz | Ctz | Popcnt | Add | Sub | Mul | And | Or | Xor | Shl
  | Shr_s | Shr_u | Rotl | Rotr | Eqz | Eq | Ne | Lt_s | Lt_u | Gt_s | Gt_u
  | Le_s | Le_u | Ge_s | Ge_u | Extend8_s | Extend16_s | Wrap_i64 | Trunc_f_s _
  | Trunc_f_u _ | Trunc_sat_f_s _ | Trunc_sat_f_u _ | Reinterpret_f _ | Load _
  | Load8_s _ | Load8_u _ | Load16_s _ | Load16_u _ | Store _ | Store8 _
  | Store16 _ ->
    ()

let check_i64 ~uuid ~invariants : Binary.i64_instr -> unit = function
  | (Div_s | Div_u | Rem_s | Rem_u) as instr ->
    check_can_divide_by_zero invariants uuid Binary.pp_i64_instr instr
  | Const _ | Clz | Ctz | Popcnt | Add | Sub | Mul | And | Or | Xor | Shl
  | Shr_s | Shr_u | Rotl | Rotr | Eqz | Eq | Ne | Lt_s | Lt_u | Gt_s | Gt_u
  | Le_s | Le_u | Ge_s | Ge_u | Extend8_s | Extend16_s | Trunc_f_s _
  | Trunc_f_u _ | Trunc_sat_f_s _ | Trunc_sat_f_u _ | Reinterpret_f _ | Load _
  | Load8_s _ | Load8_u _ | Load16_s _ | Load16_u _ | Store _ | Store8 _
  | Store16 _ | Extend32_s | Extend_i32_s | Extend_i32_u | Load32_s _
  | Load32_u _ | Store32 _ ->
    ()

let check_f32 ~uuid ~invariants : Binary.f32_instr -> unit = function
  | Div as instr ->
    check_can_divide_by_zero invariants uuid Binary.pp_f32_instr instr
  | Const _ | Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest | Add | Sub
  | Mul | Min | Max | Copysign | Eq | Ne | Lt | Gt | Le | Ge | Demote_f64
  | Convert_i_s _ | Convert_i_u _ | Reinterpret_i _ | Load _ | Store _ ->
    ()

let check_f64 ~uuid ~invariants : Binary.f64_instr -> unit = function
  | Div as instr ->
    check_can_divide_by_zero invariants uuid Binary.pp_f64_instr instr
  | Const _ | Abs | Neg | Sqrt | Ceil | Floor | Trunc | Nearest | Add | Sub
  | Mul | Min | Max | Copysign | Eq | Ne | Lt | Gt | Le | Ge | Promote_f32
  | Convert_i_s _ | Convert_i_u _ | Reinterpret_i _ | Load _ | Store _ ->
    ()

let rec check_expr (expr : Binary.expr Annotated.t)
  ~(invariants : Abstract_invariant.t) ~env ~envs =
  List.iter (check_instr ~invariants ~env ~envs) expr.raw

and check_instr (instr : Binary.instr Annotated.t)
  ~(invariants : Abstract_invariant.t) ~env ~envs =
  match instr.raw with
  | Block (_str_opt, _, expr) -> check_expr expr ~invariants ~env ~envs
  | If_else (_str_opt, _bt, expr_then, expr_else) ->
    check_expr ~invariants ~env ~envs expr_then;
    check_expr ~invariants ~env ~envs expr_else
  | Loop (_str_opt, _bt, expr) -> check_expr ~invariants ~env ~envs expr
  | Call idx ->
    let func = Link_env.get_func env idx in
    begin match func with
    | Wasm { func; idx } ->
      let env = Dynarray.get envs idx in
      check_expr ~invariants ~env ~envs func.body
    | Extern _ -> ()
    end
  | I32 i32_instr -> check_i32 ~uuid:instr.uuid ~invariants i32_instr
  | I64 i64_instr -> check_i64 ~uuid:instr.uuid ~invariants i64_instr
  | F32 f32_instr -> check_f32 ~uuid:instr.uuid ~invariants f32_instr
  | F64 f64_instr -> check_f64 ~uuid:instr.uuid ~invariants f64_instr
  | V128 _ | I8x16 _ | I16x8 _ | I32x4 _ | I64x2 _ | F32x4 _ | F64x2 _ | Ref _
  | Local _ | Global _ | Table _ | Elem _ | Memory _ | Data _ | I31 _ | Struct _
  | Array _ | Drop | Select _ | Nop | Unreachable | Br _ | Br_if _
  | Br_table (_, _)
  | Br_on_null _ | Br_on_non_null _
  | Br_on_cast (_, _, _)
  | Br_on_cast_fail (_, _, _)
  | Return | Return_call _
  | Return_call_indirect (_, _)
  | Return_call_ref _
  | Call_indirect (_, _)
  | Call_ref _ | Any_convert_extern | Extern_convert_any ->
    ()

let check_module (link_state : Abstract_extern_func.t Link.State.t)
  (m : Abstract_extern_func.t Linked.Module.t)
  (invariants : Abstract_invariant.t) =
  let envs = Link.State.get_envs link_state in
  let env = m.env in
  List.iter (check_expr ~invariants ~env ~envs) m.to_run

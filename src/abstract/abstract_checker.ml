(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let check_can_divide_by_zero invariants uuid pp instr =
  if Abstract_invariant.can_divide_by_zero invariants ~uuid then
    Log.app (fun m ->
      m "Possible division by zero for expression:(uuid: %i) %a" uuid pp instr )

let check_i32 ~uuid ~invariants : Binary.i32_instr -> unit = function
  | (Div_s | Div_u | Rem_s | Rem_u) as instr ->
    check_can_divide_by_zero invariants uuid Binary.pp_i32_instr instr
  | _ -> ()

let check_i64 ~uuid ~invariants : Binary.i64_instr -> unit = function
  | (Div_s | Div_u | Rem_s | Rem_u) as instr ->
    check_can_divide_by_zero invariants uuid Binary.pp_i64_instr instr
  | _ -> ()

let check_f32 ~uuid ~invariants : Binary.f32_instr -> unit = function
  | Div as instr ->
    check_can_divide_by_zero invariants uuid Binary.pp_f32_instr instr
  | _ -> ()

let check_f64 ~uuid ~invariants : Binary.f64_instr -> unit = function
  | Div as instr ->
    check_can_divide_by_zero invariants uuid Binary.pp_f64_instr instr
  | _ -> ()

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
  | _ -> ()

let check_module (link_state : Abstract_extern_func.extern_func Link.State.t)
  (m : Abstract_extern_func.extern_func Linked.Module.t)
  (invariants : Abstract_invariant.t) =
  let envs = Link.State.get_envs link_state in
  let env = m.env in
  List.iter (check_expr ~invariants ~env ~envs) m.to_run

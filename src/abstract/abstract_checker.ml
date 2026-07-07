(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let rec check_expr (expr : Binary.expr Annotated.t)
  ~(invariants : Abstract_invariant.t) ~env ~envs =
  List.iter (check_instr ~invariants ~env ~envs) expr.raw

and check_instr (instr : Binary.instr Annotated.t)
  ~(invariants : Abstract_invariant.t) ~env ~envs =
  if Abstract_invariant.can_divide_by_zero invariants ~uuid:instr.uuid then
    Log.app (fun m ->
      m "Possible division by zero for expression:(uuid: %i) %a" instr.uuid
        (Binary.pp_instr ~short:true)
        instr.raw );
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
  | _ -> ()

let expr (link_state : Abstract_extern_func.extern_func Link.State.t)
  (m : Abstract_extern_func.extern_func Linked.Module.t)
  (invariants : Abstract_invariant.t) =
  let envs = Link.State.get_envs link_state in
  let env = m.env in
  List.iter (check_expr ~invariants ~env ~envs) m.to_run

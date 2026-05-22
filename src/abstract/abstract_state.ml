(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { ctx : Abstract_domain.Context.t
  ; stack : Abstract_stack.t
  ; locals : Abstract_value.t Abstract_locals.t
  ; func_rt : Binary.val_type list
  ; env : Abstract_extern_func.extern_func Link_env.t
  ; envs : Abstract_extern_func.extern_func Link_env.t Dynarray.t
  }

let pp : t Fmt.t =
 fun fmt state ->
  Fmt.pf fmt "{@\n@[<hov 2>  ctx : %a,@;stack : %a,@;locals : %a@]@\n}"
    Abstract_domain.context_pretty state.ctx Abstract_stack.pp state.stack
    (Fmt.list ~sep:Fmt.semi (Abstract_value.pp_with_ctx state.ctx))
    (Abstract_locals.to_list state.locals |> List.map snd)

let empty env envs () =
  let ctx = Abstract_domain.root_context () in
  let stack = Abstract_stack.empty in
  let locals = Abstract_locals.empty in
  let func_rt = [] in
  { ctx; stack; locals; env; func_rt; envs }

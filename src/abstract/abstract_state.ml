(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { ctx : Abstract_domain.Context.t
  ; stack : Abstract_stack.t
  ; locals : Abstract_value.t Abstract_locals.t
  ; func_rt : Binary.val_type list
  ; invariant : Abstract_invariant.t
  ; call_stack : int list
  }

let pp : t Fmt.t =
 fun fmt state ->
  Fmt.pf fmt "@\n  @[<v>context: %a@\nstack  : %a@\nlocals : %a@]"
    Abstract_domain.context_pretty state.ctx
    (Abstract_stack.pp state.ctx)
    state.stack
    (Fmt.list ~sep:Fmt.semi (Abstract_value.pp_with_ctx state.ctx))
    (Abstract_locals.to_list state.locals |> List.map snd)

let empty () =
  let ctx = Abstract_domain.root_context () in
  let stack = Abstract_stack.empty in
  let locals = Abstract_locals.empty in
  let func_rt = [] in
  let invariant = Abstract_invariant.empty () in
  let call_stack = [] in
  { ctx; stack; locals; func_rt; invariant; call_stack }

let empty_exec_state ~ctx ~stack =
  let invariant = Abstract_invariant.empty () in
  let locals = Abstract_locals.empty in
  let func_rt = [] in
  let call_stack = [] in
  { ctx; stack; locals; func_rt; invariant; call_stack }

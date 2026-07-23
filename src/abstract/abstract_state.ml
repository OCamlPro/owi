(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t =
  { ctx : Abstract_domain.Context.t
  ; stack : Abstract_stack.t
  ; locals : Abstract_value.t Abstract_locals.t
  ; globals : Abstract_value.t Abstract_globals.t
  ; func_rt : Binary.val_type list
  ; invariant : Abstract_invariant.t
  }

let pp ctx : t Fmt.t =
 fun fmt state ->
  Fmt.pf fmt "@\n  @[<v>context: %a@\nstack  : %a@\nlocals : %a@]"
    Abstract_domain.context_pretty state.ctx (Abstract_stack.pp ctx) state.stack
    (Fmt.list ~sep:Fmt.semi (Abstract_value.pp_with_ctx state.ctx))
    (Abstract_locals.to_list state.locals |> List.map snd)

let init_globals ctx env =
  let f i (v : Concrete_global.t) acc =
    let v =
      match v.value with
      | I32 i -> Abstract_value.I32 (Abstract_i32.of_int32 ctx i)
      | I64 i -> Abstract_value.I64 (Abstract_i64.of_int64 ctx i)
      | F32 f -> Abstract_value.F32 (Abstract_f32.of_float32 ctx f)
      | F64 f -> Abstract_value.F64 (Abstract_f64.of_float ctx f)
      | V128 v -> Abstract_value.V128 (Abstract_v128.of_concrete ctx v)
      | _ -> assert false
    in
    Abstract_globals.add i v acc
  in
  Link_env.fold_globals f Abstract_globals.empty env

let empty env () =
  let ctx = Abstract_domain.root_context () in
  let stack = Abstract_stack.empty in
  let locals = Abstract_locals.empty in
  let globals = init_globals ctx env in
  let func_rt = [] in
  let invariant = Abstract_invariant.empty () in
  { ctx; stack; locals; func_rt; invariant; globals }

let empty_exec_state ~ctx ~locals ~env =
  let invariant = Abstract_invariant.empty () in
  let globals = init_globals ctx env in
  let stack = Abstract_stack.empty in
  { ctx; stack; locals; func_rt = []; invariant; globals }

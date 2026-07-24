(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module I = Abstract_interpreter_control_flow

let unsafe = false

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

let do_action ctx env = function
  | Wast.Invoke (module_name, func_name, args) -> begin
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        module_name func_name Wast.pp_consts args );
    let* f, modul =
      Abstract_env.get_exported_func env ~module_name ~func_name
    in
    let stack =
      List.rev_map (Abstract_value.of_script_const ctx ~ty) args
      |> List.mapi (fun i v -> (i, v))
    in
    let locals = Abstract_locals.of_list stack in
    I.exec_vfunc_from_outside ~ctx ~locals ~modul ~env f
    end
  | Get (_module_name, _name) ->
    Log.info (fun m -> m "get...");
    assert false
(* let* global = Link.get_global_from_module env mod_id name in *)
(* let v = Abstract_value.of_concrete ctx global.value in *)
(* Ok [ v ] *)

let run_one ~no_exhaustion:_
  (state : (Abstract_env.t * Abstract_domain.Context.t) Result.t) cmd =
  let* env, ctx = state in
  match cmd with
  | Wast.Text_module (false, m) ->
    let* modul, env =
      Compile.Text.until_abstract_link env ~unsafe ~name:None m
    in
    let state = I.modul_with_ctx ctx env ~modul in
    Ok (env, state.ctx)
  | Assert (Assert_return (action, res)) ->
    let* state = do_action ctx env action in
    let stack = List.rev state.stack in
    if
      List.compare_lengths res stack <> 0
      || not
           (List.for_all2
              (Abstract_value.equal_script_result ctx ~ty)
              res stack )
    then begin
      (* Log.err (fun m -> *)
      (*   m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res ); *)
      Error `Bad_result
    end
    else Ok (env, ctx)
  | _ -> assert false

let run ~no_exhaustion script =
  let state =
    Abstract_env.empty ()
    |> Abstract_env.link_extern_module ~name:"spectest_extern"
         Spectest.abstract_extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in

  let ctx = Abstract_domain.root_context () in
  List.fold_left
    (fun acc cmd -> run_one ~no_exhaustion acc cmd)
    (Ok (state, ctx))
    script

let exec ~(no_exhaustion : bool) (script : Wast.script) =
  let res = run ~no_exhaustion script in
  (* match Symex.Monad.run to_run (Thread.init ()) with *)
  match res with
  | Error _e -> Error (`Msg "script failed!")
  | Ok _ -> Ok ()

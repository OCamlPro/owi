(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module I = Abstract_interpreter_control_flow

let unsafe = false

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

let do_action env = function
  | Wast.Invoke (module_name, func_name, args) -> begin
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        module_name func_name Wast.pp_consts args );
    let* f = Env.Abstract.get_exported_func ~env ~module_name ~func_name in
    let stack = List.rev_map (Abstract_value.of_script_const ctx ~ty) args in
    I.exec_vfunc_from_outside ~ctx ~stack ~env f
    end
  | Get (_module_name, _name) ->
    Log.info (fun m -> m "get...");
    assert false
(* let* global = Link.get_global_from_module env mod_id name in *)
(* let v = Abstract_value.of_concrete ctx global.value in *)
(* Ok [ v ] *)

let run_one ~no_exhaustion:_ (state : Env.Abstract.t Result.t) cmd =
  let* env = state in
  match cmd with
  | Wast.Text_module (false, m) ->
    let* modul, env =
      Compile.Text.until_abstract_link env ~unsafe ~name:None m
    in
    let _state = I.modul_with_ctx ~env ~modul in
    (* TODO: set context in env? Or isn't it necessary as it's supposed to be mutable? *)
    Ok env
  | Assert (Assert_return (action, res)) ->
    let* state = do_action env action in
    let stack = List.rev state.stack in
    if
      List.compare_lengths res stack <> 0
      || not
           (List.for_all2
              (Abstract_value.equal_script_result
                 (Env.Abstract.get_context ~env)
                 ~ty )
              res stack )
    then begin
      (* Log.err (fun m -> *)
      (*   m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res ); *)
      Error `Bad_result
    end
    else Ok (env, ctx)
  | Register (name, modid) ->
    let+ env = Env.Abstract.register_module ~env ~name ~modid in
    (env, ctx)
  | _ -> assert false

let run ~no_exhaustion script =
  let context = Abstract_domain.root_context () in
  let env = Env.Abstract.empty ~context in
  let* state =
    Env.Abstract.link_extern_module ~env ~name:"spectest_extern"
      Spectest.abstract_extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in

  List.fold_left
    (fun acc cmd -> run_one ~no_exhaustion acc cmd)
    (Ok state) script

let exec ~(no_exhaustion : bool) (script : Wast.script) =
  let res = run ~no_exhaustion script in
  (* match Symex.Monad.run to_run (Thread.init ()) with *)
  match res with
  | Error _e -> Error (`Msg "script failed!")
  | Ok _ -> Ok ()

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
    let ctx = Env.Abstract.get_context ~env in
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
    Abstract_trace.record_wast_cmd Block_start cmd;
    let* modul, env =
      Compile.Text.until_abstract_link env ~unsafe ~name:None m
    in
    let _state = I.modul ~env ~modul in
    Abstract_trace.record_wast_cmd Block_end cmd;
    (* TODO: set context in env? Or isn't it necessary as it's supposed to be mutable? *)
    Ok env
  | Quoted_module (false, modul) ->
    Log.info (fun m -> m "*** quoted module");
    Abstract_trace.record_wast_cmd Block_start cmd;
    let* modul = Parse.Text.Inline_module.from_string modul in
    let* modul, env =
      Compile.Text.until_abstract_link env ~unsafe ~name:None modul
    in
    let _state = I.modul ~env ~modul in
    Abstract_trace.record_wast_cmd Block_end cmd;
    Ok env
  | Binary_module (false, id, modul) ->
    Log.info (fun m -> m "*** binary module");
    Abstract_trace.record_wast_cmd Block_start cmd;
    let* modul = Parse.Binary.Module.from_string modul in
    let modul = { modul with id } in
    let* modul, env =
      Compile.Binary.until_abstract_link env ~unsafe ~name:None modul
    in
    let _state = I.modul ~env ~modul in
    Abstract_trace.record_wast_cmd Block_end cmd;
    Ok env
  | Assert (Assert_malformed_binary (modul, expected)) ->
    Log.info (fun m -> m "*** assert_malformed_binary");
    let got = Parse.Binary.Module.from_string modul in
    let res = Script_error.check_result ~expected ~got in
    Abstract_trace.record_wast_cmd Step cmd;
    let+ () = res in
    env
  | Assert (Assert_malformed_quote (modul, expected)) ->
    Log.info (fun m -> m "*** assert_malformed_quote");
    let got = Parse.Text.Module.from_string modul in
    let+ () =
      match got with
      | Error got -> Script_error.check_error ~expected ~got
      | Ok modul ->
        let got = Compile.Text.until_binary ~unsafe modul in
        Script_error.check_result ~expected ~got
    in
    env
  | Assert (Assert_invalid_binary (modul, expected)) ->
    Log.info (fun m -> m "*** assert_invalid_binary");
    let got = Parse.Binary.Module.from_string modul in
    let+ () =
      match got with
      | Error got -> Script_error.check_error ~expected ~got
      | Ok modul ->
        begin match Binary_validate.modul modul with
        | Error got -> Script_error.check_error ~expected ~got
        | Ok () ->
          let got = Env.Abstract.link_binary_module ~env ~name:None ~modul in
          Script_error.check_result ~expected ~got
        end
    in
    env
  | Assert (Assert_invalid (modul, expected)) ->
    Log.info (fun m -> m "*** assert_invalid");
    let got = Compile.Text.until_abstract_link env ~unsafe ~name:None modul in
    let+ () = Script_error.check_result ~expected ~got in
    env
  | Assert (Assert_invalid_quote (modul, expected)) ->
    Log.info (fun m -> m "*** assert_invalid_quote");
    let got = Parse.Text.Script.from_string modul in
    let+ () =
      match got with
      | Error got -> Script_error.check_error ~expected ~got
      | Ok [ Text_module (false, modul) ] ->
        let got = Compile.Text.until_validate ~unsafe modul in
        Script_error.check_result ~expected ~got
      | _ -> assert false
    in
    env
  | Assert (Assert_malformed (modul, expected)) ->
    Log.info (fun m -> m "*** assert_malformed");
    let got = Compile.Text.until_abstract_link ~unsafe ~name:None env modul in
    let+ () = Script_error.check_result ~expected ~got in
    assert false
  | Assert (Assert_return (action, res)) ->
    Abstract_trace.record_wast_cmd Block_start cmd;
    let* state = do_action env action in
    let stack = List.rev state.stack in
    let res =
      if
        List.compare_lengths res stack <> 0
        || not
           @@ List.for_all2
                (Abstract_value.equal_script_result
                   (Env.Abstract.get_context ~env)
                   ~ty )
                res stack
      then begin
        let ctx = Env.Abstract.get_context ~env in
        Abstract_trace.record_wast_test_res
          (Fail
             { expected = Fmt.to_to_string Wast.pp_results res
             ; got = Fmt.to_to_string (Abstract_stack.pp ctx) stack
             } );
        Log.err (fun m ->
          m "got:      %a@;expected: %a"
            (Fmt.Dump.list (Abstract_value.pp_with_ctx ctx))
            stack Wast.pp_results res );
        Error `Bad_result
      end
      else (
        Abstract_trace.record_wast_test_res Ok;
        Ok env )
    in
    Abstract_trace.record_wast_cmd Block_end cmd;
    res
  | Assert assertion ->
    Log.warn (fun m -> m "%a is not handled" Wast.pp_assertion assertion);
    Ok env
  | Register (name, modid) ->
    Abstract_trace.record_wast_cmd Step cmd;
    let+ env = Env.Abstract.register_module ~env ~name ~modid in
    env
  | Instance _ ->
    Abstract_trace.record_wast_cmd Step cmd;
    Log.err (fun m -> m "(module instance) is not handled");
    assert false
  | Text_module (true, _) | Binary_module (true, _, _) | Quoted_module (true, _)
    ->
    Abstract_trace.record_wast_cmd Step cmd;
    Ok env
  | Action action ->
    Abstract_trace.record_wast_cmd Block_start cmd;
    let* _state = do_action env action in
    Abstract_trace.record_wast_cmd Block_end cmd;
    Ok env

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
  match res with Error _e -> Error (`Msg "script failed!") | Ok _ -> Ok ()

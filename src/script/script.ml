(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Stack = Stack.Make [@inlined hint] (Concrete_value)

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

module I = Interpret.Concrete (Interpret.Default_parameters)

let action (env : Link.Concrete.State.t) = function
  | Wast.Invoke (module_name, func_name, args) -> begin
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        module_name func_name Wast.pp_consts args );
    let* f, modul =
      Link.Concrete.State.get_exported_func env ~module_name ~func_name
    in
    let locals = List.rev_map (Concrete_value.of_script_const ~ty) args in
    I.exec_vfunc_from_outside ~locals ~modul ~env f
    end
  | Get (module_name, global_name) ->
    Log.info (fun m -> m "get...");
    let+ global =
      Link.Concrete.State.get_exported_global env ~module_name ~global_name
    in
    [ global.value ]

let unsafe = false

let run ~no_exhaustion script =
  let state =
    Link.Concrete.State.empty ()
    |> Link.Concrete.Extern.modul ~name:"spectest_extern" Spectest.extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  let registered = ref false in
  let curr_module = ref 0 in
  list_fold_left
    (fun (env : Link.Concrete.State.t) -> function
      | Wast.Text_module (false, modul) ->
        if !curr_module = 0 then
          (* TODO: disable printing*)
          ();
        Log.info (fun m -> m "*** module");
        incr curr_module;
        let* modul, env =
          Compile.Text.until_concrete_link env ~unsafe ~name:None modul
        in
        let+ () = I.modul env ~modul in
        (* TODO: enable printing again! *)
        env
      | Wast.Quoted_module (false, modul) ->
        Log.info (fun m -> m "*** quoted module");
        incr curr_module;
        let* modul = Parse.Text.Inline_module.from_string modul in
        let* modul, env =
          Compile.Text.until_concrete_link env ~unsafe ~name:None modul
        in
        let+ () = I.modul env ~modul in
        env
      | Wast.Binary_module (false, id, modul) ->
        Log.info (fun m -> m "*** binary module");
        incr curr_module;
        let* modul = Parse.Binary.Module.from_string modul in
        let modul = { modul with id } in
        let* modul, env =
          Compile.Binary.until_concrete_link env ~unsafe ~name:None modul
        in
        let+ () = I.modul env ~modul in
        env
      | Assert (Assert_trap_module (modul, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        incr curr_module;
        let* modul, env =
          Compile.Text.until_concrete_link env ~unsafe ~name:None modul
        in
        let got = I.modul env ~modul in
        let+ () = Script_error.check_result ~expected ~got in
        env
      | Assert (Assert_malformed_binary (modul, expected)) ->
        Log.info (fun m -> m "*** assert_malformed_binary");
        let got = Parse.Binary.Module.from_string modul in
        let+ () = Script_error.check_result ~expected ~got in
        env
      | Assert (Assert_malformed_quote (modul, expected)) ->
        Log.info (fun m -> m "*** assert_malformed_quote");
        (* TODO: use Parse.Text.Module.from_string instead *)
        let got = Parse.Text.Script.from_string modul in
        let+ () =
          match got with
          | Error got -> Script_error.check_error ~expected ~got
          | Ok [ Text_module (false, modul) ] ->
            let got = Compile.Text.until_binary ~unsafe modul in
            Script_error.check_result ~expected ~got
          | _ -> assert false
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
              let got = Link.Concrete.Binary.modul env ~name:None modul in
              Script_error.check_result ~expected ~got
            end
        in
        env
      | Assert (Assert_invalid (modul, expected)) ->
        Log.info (fun m -> m "*** assert_invalid");
        let got =
          Compile.Text.until_concrete_link env ~unsafe ~name:None modul
        in
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
      | Assert (Assert_unlinkable (modul, expected)) ->
        Log.info (fun m -> m "*** assert_unlinkable");
        let got =
          Compile.Text.until_concrete_link env ~unsafe ~name:None modul
        in
        let+ () = Script_error.check_result ~expected ~got in
        env
      | Assert (Assert_malformed (modul, expected)) ->
        Log.info (fun m -> m "*** assert_malformed");
        let got =
          Compile.Text.until_concrete_link ~unsafe ~name:None env modul
        in
        let+ () = Script_error.check_result ~expected ~got in
        assert false
      | Assert (Assert_return (a, res)) ->
        Log.info (fun m -> m "*** assert_return");
        let* stack = action env a in
        let stack = List.rev stack in
        if
          List.compare_lengths res stack <> 0
          || not
               (List.for_all2
                  (Concrete_value.equal_script_result ~ty)
                  res stack )
        then begin
          Log.err (fun m ->
            m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res );
          Error `Bad_result
        end
        else Ok env
      | Assert (Assert_trap (a, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        let got = action env a in
        let+ () = Script_error.check_result ~expected ~got in
        env
      | Assert (Assert_exhaustion (a, expected)) ->
        Log.info (fun m -> m "*** assert_exhaustion");
        let+ () =
          if no_exhaustion then Ok ()
          else
            let got = action env a in
            Script_error.check_result ~expected ~got
        in
        env
      | Register (name, mod_name) ->
        if !curr_module = 1 && not !registered then (* TODO: disable debug *) ();
        Log.info (fun m -> m "*** register");
        let+ state =
          Link.Concrete.State.register_last_module env ~name ~id:mod_name
        in
        (* TODO: enable debug again! *)
        state
      | Action a ->
        Log.info (fun m -> m "*** action");
        let+ _stack = action env a in
        env
      | Text_module (true, _)
      | Binary_module (true, _, _)
      | Quoted_module (true, _) ->
        (* TODO: differentiate between modules and module definitions in the
            link state, ensure that we can instantiate a module from its module
            definition, and that module definitions are not treated as "normal",
            or instantiated module. *)
        Ok env
      | Instance (_name, _mod_name) ->
        Error (`Unimplemented "(module instance _)") )
    state script

let exec ~no_exhaustion script =
  let+ _env = run ~no_exhaustion script in
  ()

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Stack = Stack.Make [@inlined hint] (Concrete_value)

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

let action (link_state : Concrete_extern_func.t Link.State.t) = function
  | Wast.Invoke (mod_id, f, args) -> begin
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        mod_id f Wast.pp_consts args );
    let* f, env = Link.State.get_func_from_module link_state mod_id f in
    let locals = List.rev_map (Concrete_value.of_script_const ~ty) args in
    let envs = Link.State.get_envs link_state in
    let module I = Interpret.Concrete (Interpret.Default_parameters) in
    I.exec_vfunc_from_outside ~locals ~env ~envs f
    end
  | Get (mod_id, name) ->
    Log.info (fun m -> m "get...");
    let+ global = Link.State.get_global_from_module link_state mod_id name in
    [ global.value ]

let unsafe = false

let run ~no_exhaustion script =
  let state =
    Link.State.empty ()
    |> Link.Extern.modul ~name:"spectest_extern" Spectest.extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  let registered = ref false in
  let curr_module = ref 0 in
  let module I = Interpret.Concrete (Interpret.Default_parameters) in
  list_fold_left
    (fun (link_state : Concrete_extern_func.t Link.State.t) -> function
      | Wast.Text_module (false, m) ->
        if !curr_module = 0 then
          (* TODO: disable printing*)
          ();
        Log.info (fun m -> m "*** module");
        incr curr_module;
        let* m, link_state =
          Compile.Text.until_link link_state ~unsafe ~name:None m
        in
        let+ () = I.modul link_state m in
        (* TODO: enable printing again! *)
        link_state
      | Wast.Quoted_module (false, m) ->
        Log.info (fun m -> m "*** quoted module");
        incr curr_module;
        let* m = Parse.Text.Inline_module.from_string m in
        let* m, link_state =
          Compile.Text.until_link link_state ~unsafe ~name:None m
        in
        let+ () = I.modul link_state m in
        link_state
      | Wast.Binary_module (false, id, m) ->
        Log.info (fun m -> m "*** binary module");
        incr curr_module;
        let* m = Parse.Binary.Module.from_string m in
        let m = { m with id } in
        let* m, link_state =
          Compile.Binary.until_link link_state ~unsafe ~name:None m
        in
        let+ () = I.modul link_state m in
        link_state
      | Assert (Assert_trap_module (m, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        incr curr_module;
        let* m, link_state =
          Compile.Text.until_link link_state ~unsafe ~name:None m
        in
        let got = I.modul link_state m in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
      | Assert (Assert_malformed_binary (m, expected)) ->
        Log.info (fun m -> m "*** assert_malformed_binary");
        let got = Parse.Binary.Module.from_string m in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
      | Assert (Assert_malformed_quote (m, expected)) ->
        Log.info (fun m -> m "*** assert_malformed_quote");
        (* TODO: use Parse.Text.Module.from_string instead *)
        let got = Parse.Text.Script.from_string m in
        let+ () =
          match got with
          | Error got -> Script_error.check_error ~expected ~got
          | Ok [ Text_module (false, m) ] ->
            let got = Compile.Text.until_binary ~unsafe m in
            Script_error.check_result ~expected ~got
          | _ -> assert false
        in
        link_state
      | Assert (Assert_invalid_binary (m, expected)) ->
        Log.info (fun m -> m "*** assert_invalid_binary");
        let got = Parse.Binary.Module.from_string m in
        let+ () =
          match got with
          | Error got -> Script_error.check_error ~expected ~got
          | Ok m ->
            begin match Binary_validate.modul m with
            | Error got -> Script_error.check_error ~expected ~got
            | Ok () ->
              let got = Link.Binary.modul link_state ~name:None m in
              Script_error.check_result ~expected ~got
            end
        in
        link_state
      | Assert (Assert_invalid (m, expected)) ->
        Log.info (fun m -> m "*** assert_invalid");
        let got = Compile.Text.until_link link_state ~unsafe ~name:None m in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
      | Assert (Assert_invalid_quote (m, expected)) ->
        Log.info (fun m -> m "*** assert_invalid_quote");
        let got = Parse.Text.Script.from_string m in
        let+ () =
          match got with
          | Error got -> Script_error.check_error ~expected ~got
          | Ok [ Text_module (false, m) ] ->
            let got = Compile.Text.until_validate ~unsafe m in
            Script_error.check_result ~expected ~got
          | _ -> assert false
        in
        link_state
      | Assert (Assert_unlinkable (m, expected)) ->
        Log.info (fun m -> m "*** assert_unlinkable");
        let got = Compile.Text.until_link link_state ~unsafe ~name:None m in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
      | Assert (Assert_malformed (m, expected)) ->
        Log.info (fun m -> m "*** assert_malformed");
        let got = Compile.Text.until_link ~unsafe ~name:None link_state m in
        let+ () = Script_error.check_result ~expected ~got in
        assert false
      | Assert (Assert_return (a, res)) ->
        Log.info (fun m -> m "*** assert_return");
        let* stack = action link_state a in
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
        else Ok link_state
      | Assert (Assert_trap (a, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        let got = action link_state a in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
      | Assert (Assert_exhaustion (a, expected)) ->
        Log.info (fun m -> m "*** assert_exhaustion");
        let+ () =
          if no_exhaustion then Ok ()
          else
            let got = action link_state a in
            Script_error.check_result ~expected ~got
        in
        link_state
      | Register (name, mod_name) ->
        if !curr_module = 1 && not !registered then (* TODO: disable debug *) ();
        Log.info (fun m -> m "*** register");
        let+ state = Link.register_last_module link_state ~name ~id:mod_name in
        (* TODO: enable debug again! *)
        state
      | Action a ->
        Log.info (fun m -> m "*** action");
        let+ _stack = action link_state a in
        link_state
      | Text_module (true, _)
      | Binary_module (true, _, _)
      | Quoted_module (true, _) ->
        (* TODO: differentiate between modules and module definitions in the
            link state, ensure that we can instantiate a module from its module
            definition, and that module definitions are not treated as "normal",
            or instantiated module. *)
        Ok link_state
      | Instance (_name, _mod_name) ->
        Error (`Unimplemented "(module instance _)") )
    state script

let exec ~no_exhaustion script =
  let+ _link_state = run ~no_exhaustion script in
  ()

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Stack = Stack.Make [@inlined hint] (Concrete_value)

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

module I = Interpret.Concrete (Interpret.Default_parameters)

let action (link_state : Concrete_extern.Func.t Link.State.t) = function
  | Wast.Invoke (mod_id, f, args) -> begin
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        mod_id f Wast.pp_consts args );
    let* f, modul = Link.State.get_func_from_module link_state mod_id f in
    let locals = List.rev_map (Concrete_value.of_script_const ~ty) args in
    I.exec_vfunc_from_outside ~locals ~modul ~link_state f
    end
  | Get (mod_id, name) ->
    Log.info (fun m -> m "get...");
    let+ global = Link.State.get_global_from_module link_state mod_id name in
    [ global.value ]

let unsafe = false

let run ~no_exhaustion script =
  let state =
    Link.State.empty ()
    |> Link.Extern.concrete_module ~name:"spectest_extern" Spectest.extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  let registered = ref false in
  let curr_module = ref 0 in
  list_fold_left
    (fun (link_state : Concrete_extern.Func.t Link.State.t) -> function
      | Wast.Text_module (false, modul) ->
        if !curr_module = 0 then
          (* TODO: disable printing*)
          ();
        Log.info (fun m -> m "*** module");
        incr curr_module;
        let* modul, link_state =
          Compile.Text.until_concrete_link link_state ~unsafe ~name:None modul
        in
        let+ () = I.modul link_state ~modul in
        (* TODO: enable printing again! *)
        link_state
      | Wast.Quoted_module (false, modul) ->
        Log.info (fun m -> m "*** quoted module");
        incr curr_module;
        let* modul = Parse.Text.Inline_module.from_string modul in
        let* modul, link_state =
          Compile.Text.until_concrete_link link_state ~unsafe ~name:None modul
        in
        let+ () = I.modul link_state ~modul in
        link_state
      | Wast.Binary_module (false, id, modul) ->
        Log.info (fun m -> m "*** binary module");
        incr curr_module;
        let* modul = Parse.Binary.Module.from_string modul in
        let modul = { modul with id } in
        let* modul, link_state =
          Compile.Binary.until_concrete_link link_state ~unsafe ~name:None modul
        in
        let+ () = I.modul link_state ~modul in
        link_state
      | Assert (Assert_trap_module (modul, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        incr curr_module;
        let* modul, link_state =
          Compile.Text.until_concrete_link link_state ~unsafe ~name:None modul
        in
        let got = I.modul link_state ~modul in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
      | Assert (Assert_malformed_binary (modul, expected)) ->
        Log.info (fun m -> m "*** assert_malformed_binary");
        let got = Parse.Binary.Module.from_string modul in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
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
        link_state
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
              let got =
                Link.Binary.concrete_module link_state ~name:None modul
              in
              Script_error.check_result ~expected ~got
            end
        in
        link_state
      | Assert (Assert_invalid (modul, expected)) ->
        Log.info (fun m -> m "*** assert_invalid");
        let got =
          Compile.Text.until_concrete_link link_state ~unsafe ~name:None modul
        in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
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
        link_state
      | Assert (Assert_unlinkable (modul, expected)) ->
        Log.info (fun m -> m "*** assert_unlinkable");
        let got =
          Compile.Text.until_concrete_link link_state ~unsafe ~name:None modul
        in
        let+ () = Script_error.check_result ~expected ~got in
        link_state
      | Assert (Assert_malformed (modul, expected)) ->
        Log.info (fun m -> m "*** assert_malformed");
        let got =
          Compile.Text.until_concrete_link ~unsafe ~name:None link_state modul
        in
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
        let+ state =
          Link.State.register_last_module link_state ~name ~id:mod_name
        in
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

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Symbolic_choice
module Stack = Stack.Make [@inlined hint] (Symbolic_value)

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

let action (link_state : Symbolic_extern_func.extern_func Link.State.t) :
  _ -> _ Symbolic_choice.t = function
  | Wast.Invoke (mod_id, f, args) -> begin
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        mod_id f Wast.pp_consts args );
    let open Syntax in
    match Link.State.get_func_from_module link_state mod_id f with
    | Error e -> trap e
    | Ok (f, env_id) -> begin
      let stack = List.rev_map (Symbolic_value.of_script_const ~ty) args in
      let envs = Link.State.get_envs link_state in
      let module I = Interpret.Symbolic (Interpret.Default_parameters) in
      I.exec_vfunc_from_outside ~locals:stack ~env:env_id ~envs f
      end
    end
  | Get (mod_id, name) -> (
    Log.info (fun m -> m "get...");
    match Link.State.get_global_from_module link_state mod_id name with
    | Error e -> trap e
    | Ok global ->
      let v = (* TODO: convert from concrete to symbolic *) global.value in
      return [ v ] )

let unsafe = false

let run ~no_exhaustion script =
  let state =
    Link.State.empty ()
    |> Link.Extern.modul ~name:"spectest_extern" Spectest.extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  let module I = Interpret.Symbolic (Interpret.Default_parameters) in
  list_fold_left
    (fun (link_state : Symbolic_extern_func.extern_func Link.State.t) ->
      function
      | Wast.Text_module (false, m) ->
        Log.info (fun m -> m "*** module");
        begin match Compile.Text.until_link link_state ~unsafe ~name:None m with
        | Error e -> trap e
        | Ok (m, link_state) ->
          let+ () = I.modul link_state m in
          (* TODO: enable printing again! *)
          link_state
        end
      | Wast.Quoted_module (false, m) ->
        Log.info (fun m -> m "*** quoted module");
        begin match Parse.Text.Inline_module.from_string m with
        | Error e -> trap e
        | Ok m ->
          begin match
            Compile.Text.until_link link_state ~unsafe ~name:None m
          with
          | Error e -> trap e
          | Ok (m, link_state) ->
            let+ () = I.modul link_state m in
            link_state
          end
        end
      | Wast.Binary_module (false, id, m) ->
        Log.info (fun m -> m "*** binary module");
        begin match Parse.Binary.Module.from_string m with
        | Error e -> trap e
        | Ok m ->
          let m = { m with id } in
          begin match
            Compile.Binary.until_link link_state ~unsafe ~name:None m
          with
          | Error e -> trap e
          | Ok (m, link_state) ->
            let+ () = I.modul link_state m in
            link_state
          end
        end
      | Assert (Assert_trap_module (m, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        begin match Compile.Text.until_link link_state ~unsafe ~name:None m with
        | Error e -> trap e
        | Ok (m, link_state) ->
          let* got = I.modul link_state m in
          begin match Script_error.check_result ~expected ~got with
          | Error e -> trap e
          | Ok () -> return link_state
          end
        end
      | Assert (Assert_malformed_binary _)
      | Assert (Assert_malformed_quote _)
      | Assert (Assert_invalid_binary _)
      | Assert (Assert_invalid _)
      | Assert (Assert_invalid_quote _)
      | Assert (Assert_unlinkable _)
      | Assert (Assert_malformed _) ->
        return link_state
      | Assert (Assert_return (a, res)) ->
        Log.info (fun m -> m "*** assert_return");
        let* stack = action link_state a in
        let stack = List.rev stack in
        if
          List.compare_lengths res stack <> 0
          || not
               (List.for_all2
                  (Symbolic_value.equal_script_result ~ty)
                  res stack )
        then begin
          Log.err (fun m ->
            m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res );
          trap `Bad_result
        end
        else return link_state
      | Assert (Assert_trap (a, expected)) ->
        Log.info (fun m -> m "*** assert_trap");
        let got = action link_state a in
        begin match Script_error.check_result ~expected ~got with
        | Error e -> trap e
        | Ok () -> return link_state
        end
      | Assert (Assert_exhaustion (a, expected)) ->
        Log.info (fun m -> m "*** assert_exhaustion");
        let+ () =
          if no_exhaustion then return ()
          else
            let got = action link_state a in
            match Script_error.check_result ~expected ~got with
            | Error e -> trap e
            | Ok () -> return ()
        in
        link_state
      | Register (name, mod_name) ->
        Log.info (fun m -> m "*** register");
        begin match Link.register_last_module link_state ~name ~id:mod_name with
        | Error e -> trap e
        | Ok state ->
          (* TODO: enable debug again! *)
          return state
        end
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
        return link_state
      | Instance (_name, _mod_name) ->
        trap (`Unimplemented "(module instance _)") )
    state script

let exec ~no_exhaustion script =
  let+ _link_state = run ~no_exhaustion script in
  ()

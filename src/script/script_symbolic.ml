(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Symbolic_choice
module Stack = Stack.Make [@inlined hint] (Symbolic_value)

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

let ( let*? ) v f = match v with Error e -> trap e | Ok v -> f v

let action (link_state : Symbolic_extern_func.extern_func Link.State.t) :
  _ -> _ Symbolic_choice.t = function
  | Wast.Invoke (mod_id, f, args) -> begin
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        mod_id f Wast.pp_consts args );
    let*? f, env_id = Link.State.get_func_from_module link_state mod_id f in
    let stack = List.rev_map (Symbolic_value.of_script_const ~ty) args in
    let envs = Link.State.get_envs link_state in
    let module I = Interpret.Symbolic (Interpret.Default_parameters) in
    I.exec_vfunc_from_outside ~locals:stack ~env:env_id ~envs f
    end
  | Get (mod_id, name) ->
    Log.info (fun m -> m "get...");
    let*? global = Link.State.get_global_from_module link_state mod_id name in
    let v = Symbolic_value.of_concrete global.value in
    return [ v ]

let unsafe = false

module I = Interpret.Symbolic (Interpret.Default_parameters)

let log_cmd : Wast.cmd -> unit =
 fun cmd ->
  let s =
    match cmd with
    | Text_module _ -> "module"
    | Quoted_module _ -> "quoted module"
    | Binary_module _ -> "binary module"
    | Assert (Assert_trap_module _) -> "assert_trap"
    | Assert (Assert_malformed_binary _)
    | Assert (Assert_malformed_quote _)
    | Assert (Assert_invalid_binary _)
    | Assert (Assert_invalid _)
    | Assert (Assert_invalid_quote _)
    | Assert (Assert_unlinkable _)
    | Assert (Assert_malformed _) ->
      "assert_{malformed,invalid,unlinkable}_..."
    | Assert (Assert_return _) -> "assert_return"
    | Assert (Assert_trap _) -> "assert_trap"
    | Assert (Assert_exhaustion _) -> "assert_exhaustion"
    | Register _ -> "register"
    | Action _ -> "action"
    | Instance (_name, _mod_name) -> "instance"
  in
  Log.info (fun m -> m "*** %s" s)

let run_one ~no_exhaustion:_
  (link_state : Symbolic_extern_func.extern_func Link.State.t Symbolic_choice.t)
  cmd =
  let* link_state in
  log_cmd cmd;
  match cmd with
  | Wast.Text_module (false, m) ->
    let*? m, link_state =
      Compile.Text.until_link link_state ~unsafe ~name:None m
    in
    let+ () = I.modul link_state m in
    link_state
  | Wast.Quoted_module (false, m) ->
    let*? m = Parse.Text.Inline_module.from_string m in
    let*? m, link_state =
      Compile.Text.until_link link_state ~unsafe ~name:None m
    in
    let+ () = I.modul link_state m in
    link_state
  | Wast.Binary_module (false, id, m) ->
    let*? m = Parse.Binary.Module.from_string m in
    let m = { m with id } in
    let*? m, link_state =
      Compile.Binary.until_link link_state ~unsafe ~name:None m
    in
    let+ () = I.modul link_state m in
    link_state
  | Assert (Assert_trap_module _) -> return link_state
  (* TODO:
let*? m, link_state =
  Compile.Text.until_link link_state ~unsafe ~name:None m
    in
    begin match I.modul link_state m with
    | Error e -> trap e
    | Ok got ->
      begin match Script_error.check_result ~expected ~got with
      | Error e -> trap e
      | Ok () -> return link_state
      end
    | _ -> assert false
    end
   end
*)
  | Assert (Assert_malformed_binary _)
  | Assert (Assert_malformed_quote _)
  | Assert (Assert_invalid_binary _)
  | Assert (Assert_invalid _)
  | Assert (Assert_invalid_quote _)
  | Assert (Assert_unlinkable _)
  | Assert (Assert_malformed _) ->
    return link_state
  | Assert (Assert_return (a, res)) ->
    let* stack = action link_state a in
    let stack = List.rev stack in
    if
      List.compare_lengths res stack <> 0
      || not (List.for_all2 (Symbolic_value.equal_script_result ~ty) res stack)
    then begin
      Log.err (fun m ->
        m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res );
      trap `Bad_result
    end
    else return link_state
  | Assert (Assert_trap _) -> return link_state
  (* TODO:
   let got = action link_state a in
   begin match Script_error.check_result ~expected ~got with
   | Error e -> trap e
   | Ok () -> return link_state
   end
*)
  | Assert (Assert_exhaustion _) -> return link_state
  (* TODO:
   let+ () =
   if no_exhaustion then return ()
   else
    let got = action link_state a in
    match Script_error.check_result ~expected ~got with
    | Error e -> trap e
    | Ok () -> return ()
   in
   link_state
*)
  | Register (name, mod_name) ->
    let*? link_state =
      Link.register_last_module link_state ~name ~id:mod_name
    in
    return link_state
  | Action a ->
    let+ _stack = action link_state a in
    link_state
  | Text_module (true, _) | Binary_module (true, _, _) | Quoted_module (true, _)
    ->
    (* TODO: differentiate between modules and module definitions in the
      link state, ensure that we can instantiate a module from its module
      definition, and that module definitions are not treated as "normal",
      or instantiated module. *)
    return link_state
  | Instance (_name, _mod_name) -> trap (`Unimplemented "(module instance _)")

let run ~no_exhaustion script =
  let state =
    Link.State.empty ()
    |> Link.Extern.modul ~name:"spectest_extern" Spectest.symbolic_extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  List.fold_left (run_one ~no_exhaustion) (return state) script

let exec ~(no_exhaustion : bool) (script : Wast.script) =
  let to_run = run ~no_exhaustion script in
  match Symex.Monad.run to_run (Thread.init ()) with
  | Error _e -> Error (`Msg "script failed!")
  | Ok (_link_state, _state) -> Ok ()
  | _ -> assert false

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Stack.Make [@inlined hint] (Symbolic_value)

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

type state = Symbolic_extern.Func.t Link.State.t * Thread.t

module I = Interpret.Symbolic (Interpret.Default_parameters)

let run_monad ~to_run ~monadic_state =
  match Symex.Monad.run to_run monadic_state with
  | Ok (v, monadic_state) -> Ok (v, monadic_state)
  | Error (`Trap t) -> Error t.Bug.err
  | Error _ -> Fmt.error_msg "unexpected error from the symbolic monad"
  | Yield _ -> Fmt.error_msg "unexpected yield from the symbolic monad"
  | Choice _ -> Fmt.error_msg "unexpected choice from the symbolic monad"

let action ((link_state, monadic_state) : state) action : _ Result.t =
  let open Syntax in
  match action with
  | Wast.Invoke (mod_id, f, args) ->
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        mod_id f Wast.pp_consts args );
    let* f, env_id = Link.State.get_func_from_module link_state mod_id f in
    let stack = List.rev_map (Symbolic_value.of_script_const ~ty) args in
    let envs = Link.State.get_envs link_state in
    let to_run = I.exec_vfunc_from_outside ~locals:stack ~env:env_id ~envs f in
    run_monad ~to_run ~monadic_state
  | Get (mod_id, name) ->
    Log.info (fun m -> m "get...");
    let* global = Link.State.get_global_from_module link_state mod_id name in
    let v = Symbolic_value.of_concrete global.value in
    Ok ([ v ], monadic_state)

let unsafe = false

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
  ~(state : Symbolic_extern.Func.t Link.State.t * Thread.t) cmd : state Result.t
    =
  let link_state, monadic_state = state in
  log_cmd cmd;
  match cmd with
  | Wast.Text_module (false, m) ->
    let open Syntax in
    let* m, link_state =
      Compile.Text.until_link link_state ~unsafe ~name:None m
    in
    let to_run = I.modul link_state m in
    let+ _got, monadic_state = run_monad ~to_run ~monadic_state in
    (link_state, monadic_state)
  | Wast.Quoted_module (false, m) ->
    let open Syntax in
    let* m = Parse.Text.Inline_module.from_string m in
    let* m, link_state =
      Compile.Text.until_link link_state ~unsafe ~name:None m
    in
    let to_run = I.modul link_state m in
    let+ _got, monadic_state = run_monad ~to_run ~monadic_state in
    (link_state, monadic_state)
  | Wast.Binary_module (false, id, m) ->
    let open Syntax in
    let* m = Parse.Binary.Module.from_string m in
    let m = { m with id } in
    let* m, link_state =
      Compile.Binary.until_link link_state ~unsafe ~name:None m
    in
    let to_run = I.modul link_state m in
    let+ _got, monadic_state = run_monad ~to_run ~monadic_state in
    (link_state, monadic_state)
  | Assert (Assert_trap_module (m, expected)) ->
    let open Syntax in
    let* m, link_state =
      Compile.Text.until_link link_state ~unsafe ~name:None m
    in
    let to_run = I.modul link_state m in
    begin match run_monad ~to_run ~monadic_state with
    | Ok ((), _monadic_state) -> Error (`Did_not_fail_but_expected expected)
    | Error got ->
      let+ () = Script_error.check_error ~expected ~got in
      (link_state, monadic_state)
    end
  | Assert (Assert_malformed_binary _)
  | Assert (Assert_malformed_quote _)
  | Assert (Assert_invalid_binary _)
  | Assert (Assert_invalid _)
  | Assert (Assert_invalid_quote _)
  | Assert (Assert_unlinkable _)
  | Assert (Assert_malformed _) ->
    Ok (link_state, monadic_state)
  | Assert (Assert_return (a, res)) ->
    let open Syntax in
    let* stack, monadic_state = action (link_state, monadic_state) a in
    let stack = List.rev stack in
    if
      List.compare_lengths res stack <> 0
      || not (List.for_all2 (Symbolic_value.equal_script_result ~ty) res stack)
    then begin
      Log.err (fun m ->
        m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res );
      Error `Bad_result
    end
    else Ok (link_state, monadic_state)
  | Assert (Assert_trap _) -> Ok (link_state, monadic_state)
  (* TODO:
     let got = action link_state a in
     begin match Script_error.check_result ~expected ~got with
     | Error e -> trap e
     | Ok () -> return link_state
     end
  *)
  | Assert (Assert_exhaustion _) -> Ok (link_state, monadic_state)
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
    let open Syntax in
    let+ link_state = Link.register_last_module link_state ~name ~id:mod_name in
    (link_state, monadic_state)
  | Action a ->
    let open Syntax in
    let+ _stack = action (link_state, monadic_state) a in
    (link_state, monadic_state)
  | Text_module (true, _) | Binary_module (true, _, _) | Quoted_module (true, _)
    ->
    (* TODO: differentiate between modules and module definitions in the
       link state, ensure that we can instantiate a module from its module
       definition, and that module definitions are not treated as "normal",
       or instantiated module. *)
    Ok (link_state, monadic_state)
  | Instance (_name, _mod_name) -> Error (`Unimplemented "(module instance _)")

let run ~no_exhaustion script : _ Result.t =
  Solver.solver_to_use := Some Smtml.Solver_type.Z3_solver;
  let link_state =
    Link.State.empty ()
    |> Link.Extern.symbolic_module ~name:"spectest_extern"
         Spectest.symbolic_extern_m
  in
  let monadic_state = Thread.init () in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  Syntax.list_fold_left
    (fun state cmd -> run_one ~no_exhaustion ~state cmd)
    (link_state, monadic_state)
    script

let exec ~(no_exhaustion : bool) (script : Wast.script) =
  match run ~no_exhaustion script with
  | Error e -> Fmt.error_msg "script failed with %s" (Result.err_to_string e)
  | Ok _ -> Ok ()

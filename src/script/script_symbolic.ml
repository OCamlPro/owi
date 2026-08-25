(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Stack = Stack.Make [@inlined hint] (Symbolic_value)

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

module I = Interpret.Symbolic (Interpret.Default_parameters)
open Syntax

let run_monad ~to_run =
  let thread = Thread.init () in
  match Symex.Monad.run to_run thread with
  | Ok (v, _monadic_state) -> Ok v
  | Error (`Trap t) -> Error t.Bug.err
  | Error _ -> Fmt.error_msg "unexpected error from the symbolic monad"
  | Yield _ -> Fmt.error_msg "unexpected yield from the symbolic monad"
  | Choice _ -> Fmt.error_msg "unexpected choice from the symbolic monad"

let action env action : _ Result.t =
  let open Syntax in
  match action with
  | Wast.Invoke (module_name, func_name, args) ->
    Log.info (fun m ->
      m "invoke %a %s %a..."
        (Fmt.option ~none:Fmt.nop Fmt.string)
        module_name func_name Wast.pp_consts args );
    let* f = Env.Symbolic.get_exported_func ~env ~module_name ~func_name in
    let stack = List.rev_map (Symbolic_value.of_script_const ~ty) args in
    let to_run = I.exec_vfunc_from_outside ~locals:stack ~env f in
    run_monad ~to_run
  | Get (module_name, global_name) ->
    Log.info (fun m -> m "get...");
    let+ global =
      Env.Symbolic.get_exported_global ~env ~module_name ~global_name
    in
    (env, [ global ])

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

let run_one ~no_exhaustion ~(env : Env.Symbolic.t) cmd : Env.Symbolic.t Result.t
    =
  log_cmd cmd;
  match cmd with
  | Wast.Text_module (false, m) ->
    let* modul, env =
      Compile.Text.until_symbolic_link env ~unsafe ~name:None m
    in
    let to_run = I.modul ~env ~modul in
    let+ _got = run_monad ~to_run in
    env
  | Wast.Quoted_module (false, m) ->
    let* m = Parse.Text.Inline_module.from_string m in
    let* modul, env =
      Compile.Text.until_symbolic_link env ~unsafe ~name:None m
    in
    let to_run = I.modul ~env ~modul in
    let+ _got = run_monad ~to_run in
    env
  | Wast.Binary_module (false, id, m) ->
    let* m = Parse.Binary.Module.from_string m in
    let m = { m with id } in
    let* modul, env =
      Compile.Binary.until_symbolic_link env ~unsafe ~name:None m
    in
    let to_run = I.modul ~env ~modul in
    let+ _got = run_monad ~to_run in
    env
  | Assert (Assert_trap_module (m, expected)) ->
    let* modul, env =
      Compile.Text.until_symbolic_link env ~unsafe ~name:None m
    in
    let to_run = I.modul ~env ~modul in
    begin match run_monad ~to_run with
    | Ok _env -> Error (`Did_not_fail_but_expected expected)
    | Error got ->
      let+ () = Script_error.check_error ~expected ~got in
      env
    end
  | Assert (Assert_malformed_binary _)
  | Assert (Assert_malformed_quote _)
  | Assert (Assert_invalid_binary _)
  | Assert (Assert_invalid _)
  | Assert (Assert_invalid_quote _)
  | Assert (Assert_unlinkable _)
  | Assert (Assert_malformed _) ->
    Ok env
  | Assert (Assert_return (a, res)) ->
    let* env, stack = action env a in
    let stack = List.rev stack in
    if
      List.compare_lengths res stack <> 0
      || not (List.for_all2 (Symbolic_value.equal_script_result ~ty) res stack)
    then begin
      Log.err (fun m ->
        m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res );
      Error `Bad_result
    end
    else Ok env
  | Assert (Assert_trap (a, expected)) ->
    let got = action env a in
    begin match Script_error.check_result ~expected ~got with
    | Error e -> Error e
    | Ok () -> Ok env
    end
  | Assert (Assert_exhaustion (a, expected)) ->
    if no_exhaustion then Ok env
    else
      let got = action env a in
      begin match Script_error.check_result ~expected ~got with
      | Error e -> Error e
      | Ok () -> Ok env
      end
  | Register (name, mod_name) ->
    let+ env = Env.Symbolic.register_module ~env ~name ~modid:mod_name in
    env
  | Action a ->
    let+ env, _stack = action env a in
    env
  | Text_module (true, _) | Binary_module (true, _, _) | Quoted_module (true, _)
    ->
    (* TODO: differentiate between modules and module definitions in the
       link state, ensure that we can instantiate a module from its module
       definition, and that module definitions are not treated as "normal",
       or instantiated module. *)
    Ok env
  | Instance (_name, _mod_name) -> Error (`Unimplemented "(module instance _)")

let run ~no_exhaustion script : _ Result.t =
  Solver.solver_to_use := Some Smtml.Solver_type.Z3_solver;
  let env = Env.Symbolic.empty in
  let* env =
    Env.Symbolic.link_extern_module ~env ~name:"spectest_extern"
      Spectest.symbolic_extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in
  list_fold_left (fun env cmd -> run_one ~no_exhaustion ~env cmd) env script

let exec ~(no_exhaustion : bool) (script : Wast.script) =
  match run ~no_exhaustion script with
  | Error e -> Fmt.error_msg "script failed with %s" (Result.err_to_string e)
  | Ok _ -> Ok ()

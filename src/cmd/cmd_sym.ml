(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Expr = Smtml.Expr
module Choice = Symbolic.P.Choice

let ( let*/ ) (t : 'a Result.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  match t with Error e -> Choice.return (Error e) | Ok x -> f x

let link_state =
  lazy
    (let func_typ = Symbolic.P.Extern_func.extern_type in
     let link_state =
       Link.extern_module' Link.empty_state ~name:"symbolic" ~func_typ
         Symbolic_wasm_ffi.symbolic_extern_module
     in
     Link.extern_module' link_state ~name:"summaries" ~func_typ
       Symbolic_wasm_ffi.summaries_extern_module )

let run_binary_modul ~unsafe ~optimize (pc : unit Result.t Choice.t)
  (m : Binary.modul) =
  let*/ m = Cmd_utils.add_main_as_start m in
  let link_state = Lazy.force link_state in

  let*/ m, link_state =
    Compile.Binary.until_link ~unsafe link_state ~optimize ~name:None m
  in
  let m = Symbolic.convert_module_to_run m in
  let c = Interpret.SymbolicP.modul link_state.envs m in
  Choice.bind pc (fun r ->
      match r with Error _ -> Choice.return r | Ok () -> c )

let run_file ~unsafe ~optimize pc filename =
  let*/ m = Parse.guess_from_file filename in
  let*/ m =
    match m with
    | Either.Left (Either.Left text_module) ->
      Compile.Text.until_binary ~unsafe text_module
    | Either.Left (Either.Right _text_scrpt) ->
      Error (`Msg "can't run symbolic interpreter on a script")
    | Either.Right binary_module -> Ok binary_module
  in
  run_binary_modul ~unsafe ~optimize pc m

(* NB: This function propagates potential errors (Result.err) occurring
   during evaluation (OS, syntax error, etc.), except for Trap and Assert,
   which are handled here. Most of the computations are done in the Result
   monad, hence the let*. *)
let cmd profiling debug unsafe optimize workers no_stop_at_failure no_values
  deterministic_result_order (workspace : Fpath.t) solver files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  (* deterministic_result_order implies no_stop_at_failure *)
  let no_stop_at_failure = deterministic_result_order || no_stop_at_failure in
  let* _created_dir = Bos.OS.Dir.create ~path:true ~mode:0o755 workspace in
  let pc = Choice.return (Ok ()) in
  let result = List.fold_left (run_file ~unsafe ~optimize) pc files in
  let thread : Thread.t = Thread.create () in
  let res_queue = Wq.init () in
  let callback = function
    | Symbolic_choice.EVal (Ok ()), _ -> ()
    | v -> Wq.push v res_queue
  in
  let join_handles =
    Symbolic_choice.run ~workers solver result thread ~callback
      ~callback_init:(fun () -> Wq.make_pledge res_queue)
      ~callback_end:(fun () -> Wq.end_pledge res_queue)
  in
  let results =
    Wq.read_as_seq res_queue ~finalizer:(fun () ->
        Array.iter Domain.join join_handles )
  in
  let print_bug = function
    | `ETrap (tr, model) ->
      Format.pp_std "Trap: %s@\n" (Trap.to_string tr);
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
    | `EAssert (assertion, model) ->
      Format.pp_std "Assert failure: %a@\n" Expr.pp assertion;
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
  in
  let rec print_and_count_failures count_acc results =
    match results () with
    | Seq.Nil -> Ok count_acc
    | Seq.Cons ((result, _thread), tl) ->
      let* is_err, model =
        let open Symbolic_choice in
        match result with
        | EAssert (assertion, model) ->
          print_bug (`EAssert (assertion, model));
          Ok (true, Some model)
        | ETrap (tr, model) ->
          print_bug (`ETrap (tr, model));
          Ok (true, Some model)
        | EVal (Ok ()) -> Ok (false, None)
        | EVal (Error e) -> Error e
      in
      let count_acc = if is_err then succ count_acc else count_acc in
      let* () =
        if not no_values then
          let model =
            match model with None -> Hashtbl.create 16 | Some m -> m
          in
          let testcase =
            List.sort compare (Smtml.Model.get_bindings model) |> List.map snd
          in
          Cmd_utils.write_testcase ~dir:workspace ~err:is_err testcase
        else Ok ()
      in
      if (not is_err) || no_stop_at_failure then
        print_and_count_failures count_acc tl
      else Ok count_acc
  in
  let results =
    if deterministic_result_order then
      results
      |> Seq.map (function (_, thread) as x ->
           (x, List.rev @@ Thread.breadcrumbs thread) )
      |> List.of_seq
      |> List.sort (fun (_, bc1) (_, bc2) ->
             List.compare Stdlib.Int32.compare bc1 bc2 )
      |> List.to_seq |> Seq.map fst
    else results
  in
  let* count = print_and_count_failures 0 results in
  if count > 0 then Error (`Found_bug count)
  else begin
    Format.pp_std "All OK";
    Ok ()
  end

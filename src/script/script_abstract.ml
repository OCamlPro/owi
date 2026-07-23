(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module I = Abstract_interpreter_control_flow

let unsafe = false

let do_action _link_state = function _ -> assert false

type host_externref = int

let ty : host_externref Type.Id.t = Type.Id.make ()

let run_one ~no_exhaustion:_
  (state :
    (Abstract_extern.Func.t Link.State.t * Abstract_domain.Context.t) Result.t
    ) cmd =
  let* link_state, ctx = state in
  match cmd with
  | Wast.Text_module (false, m) ->
    let* m, link_state =
      Compile.Text.until_link link_state ~unsafe ~name:None m
    in
    let state = I.modul_with_ctx ctx link_state m in
    Ok (link_state, state.ctx)
  | Assert (Assert_return (action, res)) ->
    let* stack = do_action link_state action in
    let stack = List.rev stack in
    if
      List.compare_lengths res stack <> 0
      || not
           (List.for_all2
              (Abstract_value.equal_script_result ctx ~ty)
              res stack )
    then begin
      (* Log.err (fun m -> *)
      (*   m "got:      %a@.expected: %a" Stack.pp stack Wast.pp_results res ); *)
      Error `Bad_result
    end
    else Ok (link_state, ctx)
  | _ -> assert false

let run ~no_exhaustion script =
  let state =
    Link.State.empty ()
    |> Link.Extern.abstract_module ~name:"spectest_extern"
         Spectest.abstract_extern_m
  in
  let script = Spectest.m :: Register ("spectest", Some "spectest") :: script in

  let ctx = Abstract_domain.root_context () in
  List.fold_left
    (fun acc cmd -> run_one ~no_exhaustion acc cmd)
    (Ok (state, ctx))
    script

let exec ~(no_exhaustion : bool) (script : Wast.script) =
  let res = run ~no_exhaustion script in
  (* match Symex.Monad.run to_run (Thread.init ()) with *)
  match res with
  | Error _e -> Error (`Msg "script failed!")
  | Ok _ -> Ok ()

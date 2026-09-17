(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let pp_model ppf model =
  Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf " ; ") Concrete_value.pp ppf model

let rec run ~rounds f : _ Result.t =
  let open Syntax in
  let* to_run = f () in
  (* we need to reset the state before each round *)
  match Concrete_choice.run to_run Concrete_state.empty with
  | (exception Fuzz_wasm_ffi.Abort state) | Ok (state, _) ->
    let model_is_empty = Concrete_state.model_is_empty state in
    if not model_is_empty then begin
      let rounds = Option.map pred rounds in
      match rounds with Some 0 -> Ok () | None | Some _ -> run ~rounds f
    end
    else begin
      (* we stop early if no bug was found and no symbol was created: it means we won't find anything!
           it should be enough to check this on the first run only, but to avoid duplicating some code, we check it on each run... *)
      Log.warn (fun m ->
        m
          "No symbol was created and I did not find any bug. Did you use the \
           right entry point and wrote your harness correctly?" );
      Ok ()
    end
  | Error (state, _e) -> begin
    let model = Concrete_state.get_model state in
    Log.app (fun m -> m "Found a bug with model: %a" pp_model model);
    Error (`Found_bug 1)
    end

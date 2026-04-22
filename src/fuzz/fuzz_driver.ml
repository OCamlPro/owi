(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let pp_model ppf model =
  Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf " ; ") Concrete_value.pp ppf model

let handle_error () =
  Log.app (fun m -> m "Found a bug with model: %a" pp_model !Fuzz_state.model);
  Error (`Found_bug 1)

let rec handle_ok ~rounds f =
  Fuzz_state.reset ();
  begin match rounds with
  | None -> run ~rounds f
  | Some 0 -> Ok ()
  | Some n -> run ~rounds:(Some (pred n)) f
  end

and run ~rounds f =
  match f () with Ok () -> handle_ok ~rounds f | Error _ -> handle_error ()

let run ~rounds f =
  match f () with
  | Ok () ->
    begin match !Fuzz_state.model with
    | [] ->
      (* on the first run, we stop early if no bug was found and no symbol was created: it means we won't find anything! *)
      Log.warn (fun m ->
        m
          "No symbol was created and I did not find any bug. Did you use the \
           right entry point and wrote your harness correctly?" );
      Ok ()
    | _model ->
      let rounds = Option.map pred rounds in
      handle_ok ~rounds f
    end
  | Error _ -> handle_error ()

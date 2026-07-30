(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let pp_model ppf model =
  Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf " ; ") Concrete_value.pp ppf model

let found_bug e =
  Log.app (fun m -> m "Found a bug with model: %a" pp_model !Fuzz_state.model);
  e

let rec run ~rounds f =
  match f () with
  | Ok () ->
    Fuzz_state.reset ();
    begin match rounds with
    | None -> run ~rounds f
    | Some 0 -> Ok ()
    | Some n -> run ~rounds:(Some (pred n)) f
    end
  | Error _ as e -> found_bug e

let run ~rounds f =
  (* First run where we stop early if no symbol is created *)
  match f () with
  | Ok () ->
    begin match !Fuzz_state.model with
    | [] ->
      (* no symbol found, the next run will be the exact same, no need to go further! *)
      Ok ()
    | _nonempty_model ->
      (* enter the real fuzzing loop *)
      let rounds = Option.map pred rounds in
      run ~rounds f
    end
  | Error _ as e -> found_bug e

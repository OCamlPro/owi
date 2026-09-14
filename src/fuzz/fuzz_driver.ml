(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let pp_model ppf model =
  Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf " ; ") Concrete_value.pp ppf model

let rec run ~rounds f =
  match f () with
  | (exception Fuzz_wasm_ffi.Abort) | Ok () ->
    begin match !Fuzz_state.model with
    | _ :: _ ->
      let rounds = Option.map pred rounds in
      Fuzz_state.reset ();
      begin match rounds with Some 0 -> Ok () | None | Some _ -> run ~rounds f
      end
    | [] ->
      (* we stop early if no bug was found and no symbol was created: it means we won't find anything!
           it should be enough to check this on the first run only, but to avoid duplicating some code, we check it on each run... *)
      Log.warn (fun m ->
        m
          "No symbol was created and I did not find any bug. Did you use the \
           right entry point and wrote your harness correctly?" );
      Ok ()
    end
  | Error _e -> begin
    Log.app (fun m -> m "Found a bug with model: %a" pp_model !Fuzz_state.model);
    Error (`Found_bug 1)
    end

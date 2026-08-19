(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let unsat_cache = ref None
let cache_file = ref (Fpath.v "unsat_cache.json")

let enable () =
  match Unsat_cache_io.load !cache_file with
  | Ok cache ->
      Logs.info (fun m -> m "Unsat cache ENABLED (loaded from %a)" Fpath.pp !cache_file);
      unsat_cache := Some cache
  | Error _ ->
      Logs.info (fun m -> m "Unsat cache ENABLED");
      unsat_cache := Some (Unsat_cache.create ())

let disable () =
  match !unsat_cache with
  | Some cache ->
      (match Unsat_cache_io.save cache !cache_file with
       | Ok () -> Logs.info (fun m -> m "Unsat cache saved to %a" Fpath.pp !cache_file)
       | Error msg -> Logs.warn (fun m -> m "Failed to save unsat cache: %s" msg))
  | None -> ();
  unsat_cache := None

let get () = !unsat_cache

let set_file file = cache_file := file [@@warning "-32"]
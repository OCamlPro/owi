(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let unsat_cache = ref None

let enable () =
  Logs.info (fun m -> m "Unsat cache ENABLED");
  unsat_cache := Some (Unsat_cache.create ())

let disable () =
  unsat_cache := None

let get () = !unsat_cache
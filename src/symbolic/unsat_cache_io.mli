(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val save : Unsat_cache.t -> Fpath.t -> (unit, string) result
val load : Fpath.t -> (Unsat_cache.t, string) result
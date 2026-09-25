(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val metadata :
  workspace:Fpath.t -> int -> Fpath.t option -> Fpath.t list -> unit Result.t

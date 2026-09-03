(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val eacsl_instrument :
  bool -> includes:Fpath.t list -> Fpath.t list -> Fpath.t list Result.t

val compile :
     workspace:Fpath.t
  -> entry_point:string option
  -> includes:Fpath.t list
  -> opt_lvl:string
  -> out_file:Fpath.t option
  -> Fpath.t list
  -> Fpath.t Result.t

val metadata :
  workspace:Fpath.t -> int -> Fpath.t option -> Fpath.t list -> unit Result.t

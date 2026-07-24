(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val env : unit -> Abstract_env.t

val cmd :
     source_file:Fpath.t
  -> entry_point:string option
  -> unsafe:bool
  -> unit Result.t

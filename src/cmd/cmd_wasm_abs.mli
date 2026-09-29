(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val env : unit -> Env.Abstract.t Result.t

val cmd :
     debug_trace:string option
  -> entry_point:string option
  -> source_file:Fpath.t
  -> unsafe:bool
  -> unit Result.t

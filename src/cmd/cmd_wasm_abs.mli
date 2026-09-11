(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val env : unit -> Env.Abstract.t Result.t

val cmd :
     source_file:Fpath.t
  -> entry_point:string option
  -> unsafe:bool
  -> debug_trace:string option
  -> unit Result.t

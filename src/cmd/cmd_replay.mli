(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val cmd :
     unsafe:bool
  -> replay_file:Fpath.t
  -> source_file:Fpath.t
  -> entry_point:string option
  -> invoke_with_symbols:bool
  -> unit Result.t

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val convert :
     Fpath.t option
  -> Fpath.t
  -> unsafe:bool
  -> Text.Module.t
  -> (unit, Result.err) result

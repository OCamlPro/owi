(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val convert :
     Fpath.t
  -> unsafe:bool
  -> optimize:bool
  -> Text.modul
  -> (unit, Result.err) result

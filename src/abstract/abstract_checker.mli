(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val check_module :
     runtime:Abstract_runtime.t
  -> modul:Abstract_runtime.modul
  -> Abstract_invariant.t
  -> unit

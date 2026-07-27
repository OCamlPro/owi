(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val check_module :
  env:Env.Abstract.t -> modul:Env.Abstract.modul -> Abstract_invariant.t -> unit

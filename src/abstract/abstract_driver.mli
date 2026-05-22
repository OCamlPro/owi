(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val expr :
     Abstract_extern_func.extern_func Link.State.t
  -> Abstract_extern_func.extern_func Linked.Module.t
  -> unit

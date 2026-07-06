(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val expr :
     Extern_type.extern_func Link.State.t
  -> Extern_type.extern_func Linked.Module.t
  -> Abstract_invariant.t

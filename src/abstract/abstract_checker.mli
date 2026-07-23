(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

val check_module :
     Abstract_extern.Func.t Link.State.t
  -> Abstract_extern.Func.t Link.Linked_module.t
  -> Abstract_invariant.t
  -> unit

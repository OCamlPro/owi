(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Link.Linked_module

type t = Concrete_extern.Func.t Link.Linked_module.t

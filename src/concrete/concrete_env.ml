(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Link.Linked_module

type t = Concrete_extern.Func.t Link.Linked_module.t

let get_memory ~modul link_state id = Link.State.get_memory link_state ~modul id

let get_data ~modul link_state id = Link.State.get_data link_state ~modul id

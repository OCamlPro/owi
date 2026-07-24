(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

let get_memory ~modul link_state id = Link.State.get_memory link_state ~modul id

let get_data ~modul link_state id = Link.State.get_data link_state ~modul id

let get_func ~modul link_state id = Link.State.get_func link_state ~modul id

let get_table ~modul link_state id = Link.State.get_table link_state ~modul id

let get_elem ~modul link_state id = Link.State.get_elem link_state ~modul id

let get_global ~modul link_state id = Link.State.get_global link_state ~modul id

let get_extern_func ~modul (link_state : Concrete_extern.Func.t Link.State.t) id
    =
  Link.State.get_extern_func link_state ~modul id

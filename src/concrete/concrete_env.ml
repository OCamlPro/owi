(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Link.Concrete.State

let get_memory ~modul env id =
  Result.ok @@ Link.Concrete.State.get_memory env ~modul id

let get_data ~modul env id = Link.Concrete.State.get_data env ~modul id

let get_func ~modul env id = Link.Concrete.State.get_func env ~modul id

let get_table ~modul env id =
  Result.ok @@ Link.Concrete.State.get_table env ~modul id

let get_elem ~modul env id = Link.Concrete.State.get_elem env ~modul id

let get_global ~modul env id =
  Result.ok @@ Link.Concrete.State.get_global env ~modul id

let get_extern_func ~modul (env : Link.Concrete.State.t) id =
  Link.Concrete.State.get_extern_func env ~modul id

let get_init_code ~modul env = Link.Concrete.State.get_init_code ~modul env

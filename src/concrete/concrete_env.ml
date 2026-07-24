(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Link.Concrete.State

let get_memory ~modul env id =
  Result.ok @@ Link.Concrete.State.get_memory env ~modul id

let get_table ~modul env id =
  Result.ok @@ Link.Concrete.State.get_table env ~modul id

let get_global ~modul env id =
  Result.ok @@ Link.Concrete.State.get_global env ~modul id

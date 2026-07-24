(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

include Link.Make (struct
  type extern_func = Concrete_extern.Func.t

  type extern_module = Concrete_extern.Module.t

  let to_func_type = Concrete_extern.Func.to_func_type

  type data = Concrete_data.t

  let data_of_concrete data = data
end)

let get_memory ~modul env id = Result.ok @@ get_memory env ~modul id

let get_table ~modul env id = Result.ok @@ get_table env ~modul id

let get_global ~modul env id = Result.ok @@ get_global env ~modul id

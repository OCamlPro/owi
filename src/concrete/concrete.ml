(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

module Extern_func = Concrete_value.Func
module Value = V
module Global = Concrete_global
module Table = Concrete_table
module Memory = Concrete_memory

type thread = unit

module Choice = Concrete_choice

let select cond ~if_true ~if_false =
  if cond then Choice.return if_true else Choice.return if_false
[@@inline]

module Elem = struct
  type t = Link_env.elem

  let get (e : t) i = e.value.(i)

  let size (e : t) = Array.length e.value
end

module Data = struct
  type t = Link_env.data

  let value data = data.Link_env.value
end

module Env = struct
  type t = Concrete_value.Func.extern_func Link_env.t

  let get_memory = Link_env.get_memory

  let get_func = Link_env.get_func

  let get_table = Link_env.get_table

  let get_elem = Link_env.get_elem

  let get_data env n =
    let data = Link_env.get_data env n in
    Choice.return data

  let get_global = Link_env.get_global

  let get_extern_func = Link_env.get_extern_func

  let drop_elem = Link_env.drop_elem

  let drop_data = Link_env.drop_data
end

module Module_to_run = struct
  (** runnable module *)
  type t = Concrete_value.Func.extern_func Link.module_to_run

  let env (t : Concrete_value.Func.extern_func Link.module_to_run) = t.env

  let modul (t : Concrete_value.Func.extern_func Link.module_to_run) = t.modul

  let to_run (t : Concrete_value.Func.extern_func Link.module_to_run) = t.to_run
end

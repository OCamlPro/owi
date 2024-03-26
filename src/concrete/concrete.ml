(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

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

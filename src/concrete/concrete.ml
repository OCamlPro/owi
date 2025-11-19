(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Extern_func = Concrete_extern_func
module Value = Concrete_value
module Global = Concrete_global
module Table = Concrete_table
module Memory = Concrete_memory
module Choice = Concrete_choice

type thread = unit

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

  let size data = String.length data.Link_env.value
end

module Env = struct
  include Link_env

  type t = Extern_func.extern_func Link_env.t

  let get_data env n =
    let data = get_data env n in
    data
end

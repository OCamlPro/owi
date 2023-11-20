(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

module P = struct
  module Extern_func = Concrete_value.Func
  module Value = V
  module Global = Concrete_global
  module Table = Concrete_table
  module Memory = Concrete_memory

  type thread = unit

  type memory = Memory.t

  type func = Concrete_value.Func.t

  type table = Table.t

  type elem = Link_env.elem

  type data = Link_env.data

  type global = Concrete_global.t

  type vbool = Bool.t

  type int32 = Int32.t

  type int64 = Int64.t

  type float32 = Float32.t

  type float64 = Float64.t

  type extern_func = Concrete_value.Func.extern_func

  type env = extern_func Link_env.t

  module Choice = struct
    type 'a t = 'a

    let return x = x [@@inline]

    let bind x f = f x [@@inline]

    let select b = b [@@inline]

    let select_i32 i = i [@@inline]

    let get = ()

    let trap msg = raise (Types.Trap msg)

    let trap : Trap.t -> 'a t = fun tr -> trap (Trap.to_string tr)
  end

  let select cond ~if_true ~if_false = if cond then if_true else if_false
  [@@inline]

  module Elem = struct
    type t = elem

    let get (e : t) i = e.value.(i)

    let size (e : t) = Array.length e.value
  end

  module Data = struct
    type t = data

    let value data = data.Link_env.value
  end

  module Env = struct
    type t = env

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

    let pp = Link_env.pp
  end

  module Module_to_run = struct
    (** runnable module *)
    type t = extern_func Link.module_to_run

    let env (t : extern_func Link.module_to_run) = t.env

    let modul (t : extern_func Link.module_to_run) = t.modul

    let to_run (t : extern_func Link.module_to_run) = t.to_run
  end
end

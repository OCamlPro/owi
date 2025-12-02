(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type Data = sig
  type t

  val value : t -> string

  val size : t -> int
end

module type Global = sig
  module Value : Value_intf.T

  type t

  val value : t -> Value.t

  val set_value : t -> Value.t -> unit
end

module type Elem = sig
  module Value : Value_intf.T

  type t

  val get : t -> int -> Value.Ref.t

  val size : t -> int
end

module type Table = sig
  module Value : Value_intf.T

  type t

  val get : t -> int -> Value.Ref.t

  val set : t -> int -> Value.Ref.t -> unit

  val size : t -> int

  val typ : t -> Text.ref_type

  val max_size : t -> int option

  val grow : t -> Int32.t -> Value.Ref.t -> unit

  val fill : t -> Int32.t -> Int32.t -> Value.Ref.t -> unit

  val copy :
    t_src:t -> t_dst:t -> src:Int32.t -> dst:Int32.t -> len:Int32.t -> unit
end

module type Env = sig
  type memory

  type data

  type global

  type elem

  type table

  type extern_func

  type 'a choice

  type t = extern_func Link_env.t

  val get_memory : t -> int -> memory choice

  val get_func : t -> int -> Kind.func

  val get_table : t -> int -> table choice

  val get_elem : t -> int -> elem

  val get_data : t -> int -> data choice

  val get_global : t -> int -> global choice

  val get_extern_func : t -> int -> extern_func

  val drop_elem : elem -> unit

  val drop_data : data -> unit
end

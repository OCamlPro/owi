(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type Data = sig
  type t

  val value : t -> string

  val size : t -> int
end

module type Memory = sig
  type t

  module Value : Value_intf.T

  module Choice : Choice_intf.Base with module V := Value

  val load_8_s : t -> Value.int32 -> Value.int32 Choice.t

  val load_8_u : t -> Value.int32 -> Value.int32 Choice.t

  val load_16_s : t -> Value.int32 -> Value.int32 Choice.t

  val load_16_u : t -> Value.int32 -> Value.int32 Choice.t

  val load_32 : t -> Value.int32 -> Value.int32 Choice.t

  val load_64 : t -> Value.int32 -> Value.int64 Choice.t

  val store_8 : t -> addr:Value.int32 -> Value.int32 -> unit Choice.t

  val store_16 : t -> addr:Value.int32 -> Value.int32 -> unit Choice.t

  val store_32 : t -> addr:Value.int32 -> Value.int32 -> unit Choice.t

  val store_64 : t -> addr:Value.int32 -> Value.int64 -> unit Choice.t

  val grow : t -> Value.int32 -> unit

  val fill : t -> pos:Value.int32 -> len:Value.int32 -> char -> unit Choice.t

  val blit :
    t -> src:Value.int32 -> dst:Value.int32 -> len:Value.int32 -> unit Choice.t

  val blit_string :
    t -> string -> src:Value.int32 -> dst:Value.int32 -> len:Value.int32 -> unit

  val size : t -> Value.int32

  val size_in_pages : t -> Value.int32

  val get_limit_max : t -> Value.int64 option
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

  val get : t -> int -> Value.ref_value

  val size : t -> int
end

module type Table = sig
  module Value : Value_intf.T

  type t

  val get : t -> int -> Value.ref_value

  val set : t -> int -> Value.ref_value -> unit

  val size : t -> int

  val typ : t -> Text.ref_type

  val max_size : t -> int option

  val grow : t -> int32 -> Value.ref_value -> unit

  val fill : t -> int32 -> int32 -> Value.ref_value -> unit

  val copy : t_src:t -> t_dst:t -> src:int32 -> dst:int32 -> len:int32 -> unit
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

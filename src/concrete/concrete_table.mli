(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime table *)

type table = Concrete_value.ref_value array

type t =
  { id : int
  ; label : string option
  ; limits : Binary.limits
  ; typ : Binary.ref_type
  ; mutable data : table
  }

val backup : t -> t

val recover : from_:t -> to_:t -> unit

val get : t -> int -> Concrete_value.ref_value

val set : t -> int -> Concrete_value.ref_value -> unit

val size : t -> int

val typ : t -> Binary.ref_type

val update : t -> table -> unit

val init : ?label:string -> Binary.table_type -> t

val max_size : t -> int option

val grow : t -> int32 -> Concrete_value.ref_value -> unit

val fill : t -> int32 -> int32 -> Concrete_value.ref_value -> unit

val copy : t_src:t -> t_dst:t -> src:int32 -> dst:int32 -> len:int32 -> unit

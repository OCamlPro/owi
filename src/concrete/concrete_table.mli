(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime table *)

type table = Concrete_value.Ref.t array

type t =
  { id : int
  ; label : string option
  ; limits : Text.limits
  ; typ : Text.ref_type
  ; mutable data : table
  }

val backup : t -> t

val recover : from_:t -> to_:t -> unit

val get : t -> int -> Concrete_value.Ref.t

val set : t -> int -> Concrete_value.Ref.t -> unit

val size : t -> int

val typ : t -> Text.ref_type

val update : t -> table -> unit

val init : ?label:string -> Text.Table.Type.t -> t

val max_size : t -> int option

val grow : t -> Concrete_value.i32 -> Concrete_value.Ref.t -> unit

val fill :
  t -> Concrete_value.i32 -> Concrete_value.i32 -> Concrete_value.Ref.t -> unit

val copy :
     t_src:t
  -> t_dst:t
  -> src:Concrete_value.i32
  -> dst:Concrete_value.i32
  -> len:Concrete_value.i32
  -> unit

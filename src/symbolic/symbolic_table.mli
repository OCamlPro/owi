(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single table *)
type t

val get : t -> int -> Symbolic_value.ref_value

val set : t -> int -> Symbolic_value.ref_value -> unit

val grow : t -> int32 -> Symbolic_value.ref_value -> unit

val fill : t -> int32 -> int32 -> Symbolic_value.ref_value -> unit

val copy : t_src:t -> t_dst:t -> src:int32 -> dst:int32 -> len:int32 -> unit

val size : t -> int

val typ : t -> Types.binary Types.ref_type

val max_size : t -> int option

(** Collection of tables *)
type collection

val init : unit -> collection

val clone : collection -> collection

val get_table : Env_id.t -> Concrete_table.t -> collection -> int -> t

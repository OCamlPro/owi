(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** runtime memory *)
type t

val get_limit_max : t -> int64 option

val get_limits : t -> Types.limits

val init : Types.limits -> t

val update_memory : t -> bytes -> unit

val load_8_s : t -> int32 -> int32

val load_8_u : t -> int32 -> int32

val load_16_s : t -> int32 -> int32

val load_16_u : t -> int32 -> int32

val load_32 : t -> int32 -> int32

val load_64 : t -> int32 -> int64

val store_8 : t -> addr:int32 -> int32 -> unit

val store_16 : t -> addr:int32 -> int32 -> unit

val store_32 : t -> addr:int32 -> int32 -> unit

val store_64 : t -> addr:int32 -> int64 -> unit

val grow : t -> int32 -> unit

val fill : t -> pos:int32 -> len:int32 -> char -> bool

val blit : t -> src:int32 -> dst:int32 -> len:int32 -> bool

val blit_string : t -> string -> src:int32 -> dst:int32 -> len:int32 -> bool

val size_in_pages : t -> int32

val size : t -> int32

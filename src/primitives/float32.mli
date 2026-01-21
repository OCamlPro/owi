(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Custom Float32 module for Wasm. *)

type t

val neg_nan : t

val pos_nan : t

val is_neg_nan : t -> bool

val is_pos_nan : t -> bool

val of_bits : Int32.t -> t

val to_bits : t -> Int32.t

val zero : t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val neg : t -> t

val abs : t -> t

val sqrt : t -> t

val ceil : t -> t

val floor : t -> t

val trunc : t -> t

val nearest : t -> t

val min : t -> t -> t

val max : t -> t -> t

val copy_sign : t -> t -> t

val eq : t -> t -> bool

val ne : t -> t -> bool

val lt : t -> t -> bool

val gt : t -> t -> bool

val le : t -> t -> bool

val ge : t -> t -> bool

val of_string_exn : string -> t

val of_string : string -> t option

val to_hex_string : t -> string

val to_string : t -> string

val to_float : t -> Float.t

val of_float : Float.t -> t

val pp : t Fmt.t

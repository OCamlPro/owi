(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = int

val wrap_i32 : Int32.t -> t

val to_int_s : t -> int

val to_int_u : t -> int

val add : t -> t -> t

val sub : t -> t -> t

val neg : t -> t

val abs : t -> t

val popcnt : t -> t

val eq : t -> t -> bool

val ne : t -> t -> bool

val lt_s : t -> t -> bool

val lt_u : t -> t -> bool

val le_s : t -> t -> bool

val le_u : t -> t -> bool

val gt_s : t -> t -> bool

val gt_u : t -> t -> bool

val ge_s : t -> t -> bool

val ge_u : t -> t -> bool

val shl : t -> int -> t

val shr_s : t -> int -> t

val shr_u : t -> int -> t

val min_s : t -> t -> t

val min_u : t -> t -> t

val max_s : t -> t -> t

val max_u : t -> t -> t

val add_sat_s : t -> t -> t

val add_sat_u : t -> t -> t

val sub_sat_s : t -> t -> t

val sub_sat_u : t -> t -> t

val avgr_u : t -> t -> t

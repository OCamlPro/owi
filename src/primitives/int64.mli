(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
(*  Written by Léo Andrès and Pierre Chambart                                *)
(*                                                                           *)
(*  SPDX-License-Identifier: AGPL-3.0-or-later                               *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU Affero General Public License as published *)
(*  by the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU Affero General Public License for more details.                      *)
(*                                                                           *)
(*  You should have received a copy of the GNU Affero General Public License *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

(** Custom Int64 module for Wasm. *)

type t = int64

val min_int : t

val max_int : t

val zero : t

(** conversion *)

val bits_of_float : float -> t

val float_of_bits : t -> float

val of_float : float -> t

val to_float : t -> float

val of_string : string -> t

val of_int : int -> t

val to_int : t -> int

val of_int32 : int32 -> t

val to_int32 : t -> int32

val extend_s : int -> t -> t

(** unary operators *)

val abs : t -> t

val clz : t -> t

val ctz : t -> t

val popcnt : t -> t

val lognot : t -> t

(** comparison operators *)

val eq : t -> t -> bool

val ne : t -> t -> bool

val lt : t -> t -> bool

val gt : t -> t -> bool

val lt_u : t -> t -> bool

val gt_u : t -> t -> bool

val le : t -> t -> bool

val ge : t -> t -> bool

val le_u : t -> t -> bool

val ge_u : t -> t -> bool

(** binary operators *)

val logor : t -> t -> t

val logand : t -> t -> t

val logxor : t -> t -> t

val rotl : t -> t -> t

val rotr : t -> t -> t

val shift_left : t -> int -> t

val shl : t -> t -> t

val shift_right : t -> int -> t

val shr_s : t -> t -> t

val shr_u : t -> t -> t

val shift_right_logical : t -> int -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val unsigned_div : t -> t -> t

val rem : t -> t -> t

val unsigned_rem : t -> t -> t

val eq_const : t -> int64 -> bool

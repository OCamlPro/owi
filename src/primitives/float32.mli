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

(** Custom Float32 module for Wasm. *)

type t

val neg_nan : t

val pos_nan : t

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

val of_string : string -> t

val to_hex_string : t -> string

val to_string : t -> string

val to_float : t -> Float.t

val of_float : Float.t -> t

val pp : Format.formatter -> t -> unit

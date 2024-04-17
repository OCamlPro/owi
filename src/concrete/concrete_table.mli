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

(** runtime table *)

open Types

type table = Concrete_value.ref_value array

type t =
  { id : int
  ; label : string option
  ; limits : limits
  ; typ : simplified ref_type
  ; mutable data : table
  }

val get : t -> int -> Concrete_value.ref_value

val set : t -> int -> Concrete_value.ref_value -> unit

val size : t -> int

val typ : t -> simplified ref_type

val update : t -> table -> unit

val init : ?label:string -> simplified table_type -> t

val max_size : t -> int option

val grow : t -> int32 -> Concrete_value.ref_value -> unit

val fill : t -> int32 -> int32 -> Concrete_value.ref_value -> unit

val copy : t_src:t -> t_dst:t -> src:int32 -> dst:int32 -> len:int32 -> unit

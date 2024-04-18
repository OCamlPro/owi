(*****************************************************************************)
(*                                                                           *)
(*  Owi                                                                      *)
(*                                                                           *)
(*  Copyright (C) 2021-2024 OCamlPro                                         *)
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

(** Module to enable or disable the printing of debug logs. *)

(** wether debug printing is enabled or not *)
val debug_on : bool ref

(** wether profiling printing is enabled or not *)
val profiling_on : bool ref

(** print some debug info *)
val debug0 : (unit, Format.formatter, unit) format -> unit

val debug1 : ('a -> unit, Format.formatter, unit) format -> 'a -> unit

val debug2 :
  ('a -> 'b -> unit, Format.formatter, unit) format -> 'a -> 'b -> unit

val debug5 :
     ('a -> 'b -> 'c -> 'd -> 'e -> unit, Format.formatter, unit) format
  -> 'a
  -> 'b
  -> 'c
  -> 'd
  -> 'e
  -> unit

(** print some profiling info *)
val profile3 :
     ('a -> 'b -> 'c -> unit, Format.formatter, unit) format
  -> 'a
  -> 'b
  -> 'c
  -> unit

(** print some error and exit *)
val err : ('a, Format.formatter, unit, 'b) format4 -> 'a

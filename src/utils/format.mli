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

type formatter = Stdlib.Format.formatter

val pp : formatter -> ('a, formatter, unit) format -> 'a

val pp_err : ('a, formatter, unit) format -> 'a

val pp_std : ('a, formatter, unit) format -> 'a

val pp_nothing : formatter -> unit -> unit

val pp_space : formatter -> unit -> unit

val pp_bool : formatter -> bool -> unit

val pp_char : formatter -> char -> unit

val pp_int : formatter -> int -> unit

val pp_flush : formatter -> unit -> unit

val pp_list :
     ?pp_sep:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a list
  -> unit

val pp_array :
     ?pp_sep:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a array
  -> unit

val pp_iter :
     ?pp_sep:(formatter -> unit -> unit)
  -> (('a -> unit) -> 'b -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'b
  -> unit

val pp_string : formatter -> string -> unit

val pp_option :
     ?none:(formatter -> unit -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> 'a option
  -> unit

val pp_newline : formatter -> unit -> unit

val sprintf : ('a, unit, string) format -> 'a

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b

val asprintf : ('a, formatter, unit, string) format4 -> 'a

val kasprintf : (string -> 'a) -> ('b, formatter, unit, 'a) format4 -> 'b

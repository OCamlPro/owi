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

include Stdlib.Format

let pp = fprintf

let pp_err = eprintf

let pp_std = printf

let pp_nothing _fmt () = ()

let pp_char = pp_print_char

let pp_list = pp_print_list

let pp_array = pp_print_array

let pp_iter = pp_print_iter

let pp_string = pp_print_string

let pp_option = pp_print_option

let pp_bool = pp_print_bool

let pp_flush = pp_print_flush

let pp_space fmt () = pp_string fmt " "

let pp_newline fmt () = pp fmt "@\n"

let pp_int = pp_print_int

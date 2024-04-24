(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

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

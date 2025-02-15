(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type scope =
  | Global
  | Process
  | Thread

val init_logger_to_file : Fpath.t -> unit

val event :
     ?scope:scope
  -> ?arg_writter:(Prelude.out_channel -> 'a) option
  -> string
  -> string
  -> unit

val start_span :
  ?arg_writter:(Prelude.out_channel -> 'a) option -> string -> string -> unit

val close_span : unit -> unit

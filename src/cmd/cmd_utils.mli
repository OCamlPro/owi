(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val write_testcase :
  dir:Fpath.t -> err:bool -> Smtml.Value.t list -> unit Result.t

val add_main_as_start : Binary.modul -> (Binary.modul, [> `Msg of string ]) result

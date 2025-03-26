(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

val from_string : string -> Binary.Module.t Result.t

val from_channel : in_channel -> Binary.Module.t Result.t

val from_file : Fpath.t -> Binary.Module.t Result.t

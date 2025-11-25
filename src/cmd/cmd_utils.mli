(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(* test-case stuff *)
val write_testcase : dir:Fpath.t -> Smtml.Value.t list -> unit Result.t

(* harness stuff *)
val set_entry_point :
     string option
  -> bool
  -> Binary.Module.t
  -> (Binary.Module.t, [> `Msg of string ]) result

(* installed files *)
val c_files_location : Fpath.t list

val zig_files_location : Fpath.t list

val find_installed_c_file : Fpath.t -> Fpath.t Result.t

val find_installed_rust_file : Fpath.t -> Fpath.t Result.t

val find_installed_zig_file : Fpath.t -> Fpath.t Result.t

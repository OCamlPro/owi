(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** The `spectest` module, to run script from the official test suite. *)

(* OCaml definition *)
val extern_m : Concrete_extern_func.extern_func Extern.Module.t

(* TODO: this can probably be removed by directly registering the previous module in script.ml ? *)

(** the spectest module, which is simply a Wast Text module importing everything
    required *)
val m : Wast.cmd

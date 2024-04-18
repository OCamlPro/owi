(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** The `spectest` module, to run script from the official test suite. *)

type extern_module = Concrete_value.Func.extern_func Link.extern_module

val extern_m : extern_module

(** the spectest module *)
val m : Text.cmd

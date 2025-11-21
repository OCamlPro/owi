(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

type collection

val init : unit -> collection

val clone : collection -> collection

val get_global : int -> Concrete_global.t -> collection -> int -> t

val value : t -> Symbolic_value.t

val set_value : t -> Symbolic_value.t -> unit

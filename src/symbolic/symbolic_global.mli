(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

include Global_intf.T with module Value := Symbolic_value and type t := t

type collection

val init : unit -> collection

val clone : collection -> collection

val get_global : int -> Concrete_global.t -> collection -> int -> t

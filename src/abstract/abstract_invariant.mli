(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

val empty : unit -> t

val cant_divide_by_zero : t -> int -> bool

val add_cant_divide_by_zero : t -> int -> unit

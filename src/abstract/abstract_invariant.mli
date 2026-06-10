(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

val pp : t Fmt.t

val empty : unit -> t

val can_divide_by_zero : t -> uuid:int -> bool

val add_divide_by_zero_invariant : t -> uuid:int -> possible:bool -> unit

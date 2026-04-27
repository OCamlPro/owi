(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t = int

val of_i8x2 : Concrete_i8.t -> Concrete_i8.t -> t

val add : t -> t -> t

val sub : t -> t -> t

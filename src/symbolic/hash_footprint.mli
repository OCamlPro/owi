(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

(** Hash footprint of an SMT formula. *)

type t

val of_expr : Smtml.Expr.t -> t
val equal : t -> t -> bool
val hash : t -> int
val of_hash : int -> t
val pp : t Fmt.t
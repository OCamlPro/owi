(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

type t

val pp : Abstract_domain.Context.t -> t Fmt.t

(** Construction and conversion functions *)

val zero : Abstract_domain.Context.t -> t

val unknown : Abstract_domain.Context.t -> t

val of_int : Abstract_domain.Context.t -> int -> t

val of_int64 : Abstract_domain.Context.t -> int64 -> t

val of_binary : Abstract_domain.Binary.t -> t

val to_binary : t -> Abstract_domain.Binary.t

val of_boolean : Abstract_domain.Context.t -> Abstract_boolean.t -> t

val to_boolean : Abstract_domain.Context.t -> t -> Abstract_boolean.t

(** Comparison functions *)

val eqz : Abstract_domain.Context.t -> t -> Abstract_boolean.t

val eq : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val ne : Abstract_domain.Context.t -> t -> t -> Abstract_domain.boolean

(* *)

val add : Abstract_domain.Context.t -> t -> t -> t

val sub : Abstract_domain.Context.t -> t -> t -> t

val mul : Abstract_domain.Context.t -> t -> t -> t

val div_s : Abstract_domain.Context.t -> t -> t -> t

val div_u : Abstract_domain.Context.t -> t -> t -> t

val rem_s : Abstract_domain.Context.t -> t -> t -> t

val rem_u : Abstract_domain.Context.t -> t -> t -> t

val and_ : Abstract_domain.Context.t -> t -> t -> t

val or_ : Abstract_domain.Context.t -> t -> t -> t

val xor : Abstract_domain.Context.t -> t -> t -> t

val lt_s : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val lt_u : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val le_s : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val le_u : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val gt_s : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val gt_u : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val ge_s : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val ge_u : Abstract_domain.Context.t -> t -> t -> Abstract_boolean.t

val shl : Abstract_domain.Context.t -> t -> t -> t

val extend_s : Abstract_domain.Context.t -> int -> t -> t

val extend_i32_u : Abstract_domain.Context.t -> t -> t

val extend_i32_s : Abstract_domain.Context.t -> t -> t

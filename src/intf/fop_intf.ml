(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type boolean

  type i32

  type i64

  type same_size_int

  val zero : t

  val abs : t -> t

  val neg : t -> t

  val sqrt : t -> t

  val ceil : t -> t

  val floor : t -> t

  val trunc : t -> t

  val nearest : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t

  val min : t -> t -> t

  val max : t -> t -> t

  val copy_sign : t -> t -> t

  val eq : t -> t -> boolean

  val ne : t -> t -> boolean

  val lt : t -> t -> boolean

  val gt : t -> t -> boolean

  val le : t -> t -> boolean

  val ge : t -> t -> boolean

  val convert_i32_s : i32 -> t

  val convert_i32_u : i32 -> t

  val convert_i64_s : i64 -> t

  val convert_i64_u : i64 -> t

  val of_bits : same_size_int -> t

  val to_bits : t -> same_size_int
end

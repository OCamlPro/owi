(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  val pp : t Fmt.t

  type boolean

  type i8

  type i16

  type i32

  type i64

  val zero : t

  val of_concrete : Concrete_v128.t -> t

  val logor : t -> t -> t

  val logand : t -> t -> t

  val any_true : t -> boolean

  module I8x16 : sig
    val eq : t -> t -> t

    val splat : i8 -> t

    val bitmask : t -> i32

    val add : t -> t -> t

    val sub : t -> t -> t
  end

  module I16x8 : sig
    val eq : t -> t -> t

    val splat : i16 -> t

    val bitmask : t -> i32

    val add : t -> t -> t

    val sub : t -> t -> t
  end

  module I32x4 : sig
    val eq : t -> t -> t

    val splat : i32 -> t

    val bitmask : t -> i32

    val add : t -> t -> t

    val sub : t -> t -> t
  end

  module I64x2 : sig
    val eq : t -> t -> t

    val splat : i64 -> t

    val bitmask : t -> i32

    val add : t -> t -> t

    val sub : t -> t -> t
  end
end

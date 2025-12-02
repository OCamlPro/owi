(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type i32

  val false_ : t

  val true_ : t

  val of_concrete : Bool.t -> t

  val not : t -> t

  val or_ : t -> t -> t

  val and_ : t -> t -> t

  val to_i32 : t -> i32

  val pp : Format.formatter -> t -> unit
end

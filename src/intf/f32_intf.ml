(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type boolean

  type i32

  type i64

  type f64

  include
    Fop_intf.T
      with type t := t
       and type boolean := boolean
       and type i32 := i32
       and type i64 := i64
       and type same_size_int := i32

  val demote_f64 : f64 -> t

  val reinterpret_i32 : i32 -> t

  val of_concrete : Float32.t -> t

  val pp : t Fmt.t
end

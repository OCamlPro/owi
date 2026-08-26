(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type boolean

  type i32

  type 'value t

  val get_type : 'value t -> int option

  val new_fill : int -> 'value -> i32 -> 'value t

  val new_fixed_with : int -> 'value array -> 'value t

  val get_elem : 'value t -> i32 -> 'value

  val set_elem : 'value t -> i32 -> 'value -> unit

  val length : 'value t -> i32

  val phys_equal : 'value t -> 'value t -> boolean
end

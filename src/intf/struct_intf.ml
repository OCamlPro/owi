(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type boolean

  type 'value t

  val get_type : 'a t -> int option

  val new_with : int -> 'value array -> 'value t

  val get_field : 'value t -> int -> 'value

  val set_field : 'value t -> int -> 'value -> unit

  val phys_equal : 'value t -> 'value t -> boolean
end

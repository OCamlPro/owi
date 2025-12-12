(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type reference

  type t

  val get : t -> int -> reference

  val size : t -> int

  val drop : t -> unit
end

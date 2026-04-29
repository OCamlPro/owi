(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type reference

  type 'a choice

  type t

  val get : t -> int -> reference

  val set : t -> int -> reference -> unit choice

  val size : t -> int

  val typ : t -> Binary.ref_type

  val max_size : t -> int option

  val grow : t -> Int32.t -> reference -> unit choice

  val fill : t -> Int32.t -> Int32.t -> reference -> unit choice

  val copy :
       t_src:t
    -> t_dst:t
    -> src:Int32.t
    -> dst:Int32.t
    -> len:Int32.t
    -> unit choice
end

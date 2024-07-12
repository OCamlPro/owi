(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

type collection

val address : unit -> Smtml.Expr.t Symbolic_choice_without_memory.t

val init : unit -> collection

val clone : collection -> collection

val get_memory : Env_id.t -> Concrete_memory.t -> collection -> int -> t

val check_within_bounds :
  t -> Smtml.Expr.t -> (Smtml.Expr.t * Symbolic_value.int32, Trap.t) result

val replace_size : t -> Int32.t -> Smtml.Expr.t -> unit

val free : t -> Int32.t -> unit

val load_8_s : t -> Smtml.Expr.t -> Symbolic_value.int32

val load_8_u : t -> Smtml.Expr.t -> Symbolic_value.int32

val load_16_s : t -> Smtml.Expr.t -> Symbolic_value.int32

val load_16_u : t -> Smtml.Expr.t -> Symbolic_value.int32

val load_32 : t -> Smtml.Expr.t -> Symbolic_value.int32

val load_64 : t -> Smtml.Expr.t -> Symbolic_value.int32

val store_8 : t -> addr:Smtml.Expr.t -> Smtml.Expr.t -> unit

val store_16 : t -> addr:Smtml.Expr.t -> Smtml.Expr.t -> unit

val store_32 : t -> addr:Smtml.Expr.t -> Smtml.Expr.t -> unit

val store_64 : t -> addr:Smtml.Expr.t -> Smtml.Expr.t -> unit

val grow : t -> Smtml.Expr.t -> unit

val fill : t -> pos:Smtml.Expr.t -> len:Smtml.Expr.t -> char -> Smtml.Expr.t

val blit :
  t -> src:Smtml.Expr.t -> dst:Smtml.Expr.t -> len:Smtml.Expr.t -> Smtml.Expr.t

val blit_string :
     t
  -> string
  -> src:Smtml.Expr.t
  -> dst:Smtml.Expr.t
  -> len:Smtml.Expr.t
  -> Smtml.Expr.t

val size : t -> Smtml.Expr.t

val size_in_pages : t -> Smtml.Expr.t

val get_limit_max : t -> Smtml.Expr.t option

module ITbl : sig
  type 'a t

  type key

  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

val iter : (t ITbl.t -> unit) -> collection -> unit

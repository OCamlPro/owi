(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

(** Single memory *)
type t

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

val blit_string :
     t
  -> string
  -> src:Smtml.Expr.t
  -> dst:Smtml.Expr.t
  -> len:Smtml.Expr.t
  -> unit

val size : t -> Smtml.Expr.t

val size_in_pages : t -> Smtml.Expr.t

val get_limit_max : t -> Smtml.Expr.t option

(** Collection of memories *)

type collection

val init : unit -> collection

val get_memory : Env_id.t -> Concrete_memory.t -> collection -> int -> t

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type M = sig
  type collection

  val init : unit -> collection

  val clone : collection -> collection
end

module type S = sig
  type t

  module Memory : M

  val init : unit -> t

  val create :
       int
    -> Smtml.Symbol.t list
    -> Symbolic_value.bool list
    -> bool
    -> Memory.collection
    -> Symbolic_table.collection
    -> Symbolic_global.collection
    -> int32 list
    -> t

  val pc : t -> Symbolic_value.bool list

  val pc_is_fresh : t -> bool

  val mark_pc_fresh : t -> t

  val memories : t -> Memory.collection

  val tables : t -> Symbolic_table.collection

  val globals : t -> Symbolic_global.collection

  val breadcrumbs : t -> int32 list

  val symbols_set : t -> Smtml.Symbol.t list

  val symbols : t -> int

  val clone : t -> t

  val add_pc : t -> Symbolic_value.bool -> t

  val add_breadcrumb : t -> int32 -> t

  val add_symbol : t -> Smtml.Symbol.t -> t

  val incr_symbols : t -> t
end

module type Intf = sig
  module type M = M

  module type S = S

  module Make (Symbolic_memory : M) :
    S with type Memory.collection = Symbolic_memory.collection
end

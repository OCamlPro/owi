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
    -> Memory.collection
    -> Symbolic_table.collection
    -> Symbolic_global.collection
    -> int32 list
    -> (int * string) list
    -> t

  val pc : t -> Symbolic_value.bool list

  val memories : t -> Memory.collection

  val tables : t -> Symbolic_table.collection

  val globals : t -> Symbolic_global.collection

  val breadcrumbs : t -> int32 list

  val symbols_set : t -> Smtml.Symbol.t list

  val symbols : t -> int

  val labels : t -> (int * string) list

  val clone : t -> t

  val add_pc : t -> Symbolic_value.bool -> t

  val add_breadcrumb : t -> int32 -> t

  val add_symbol : t -> Smtml.Symbol.t -> t

  val add_label : t -> int * string -> t

  val incr_symbols : t -> t
end

module type Intf = sig
  module type M = M

  module type S = S

  module Make (Symbolic_memory : M) :
    S with type Memory.collection = Symbolic_memory.collection
end

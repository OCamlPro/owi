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
    -> Scoped_symbol.scope_token list
    -> Symbolic_path_condition.t
    -> Memory.collection
    -> Symbolic_table.collection
    -> Symbolic_global.collection
    -> int list
    -> (int * string) list
    -> t

  val pc : t -> Symbolic_path_condition.t

  val memories : t -> Memory.collection

  val tables : t -> Symbolic_table.collection

  val globals : t -> Symbolic_global.collection

  val breadcrumbs : t -> int list

  val scoped_symbols : t -> Scoped_symbol.scope_token list

  val num_symbols : t -> int

  val labels : t -> (int * string) list

  val clone : t -> t

  val add_pc : t -> Symbolic_value.bool -> t

  val add_breadcrumb : t -> int -> t

  val add_symbol : t -> Smtml.Symbol.t -> t

  val add_label : t -> int * string -> t

  val open_scope : t -> string -> t

  val end_scope : t -> t

  val incr_num_symbols : t -> t
end

module type Intf = sig
  module Make (Symbolic_memory : M) :
    S with type Memory.collection = Symbolic_memory.collection
end

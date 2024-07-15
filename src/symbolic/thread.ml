(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type S = sig
  type t

  type memories

  val pc : t -> Symbolic_value.vbool list

  val memories : t -> memories

  val tables : t -> Symbolic_table.collection

  val globals : t -> Symbolic_global.collection

  val breadcrumbs : t -> int32 list

  val symbols_set : t -> Smtml.Symbol.t list

  val symbols : t -> int

  val create : unit -> t

  val clone : t -> t

  val add_pc : t -> Symbolic_value.vbool -> t

  val add_breadcrumb : t -> int32 -> t

  val add_symbol : t -> Smtml.Symbol.t -> t

  val incr_symbols : t -> t
end

module Make (M : Symbolic_memory_intf.S) = struct
  type memories = M.collection

  module Memory : Symbolic_memory_intf.S with type collection = memories = M

  type t =
    { symbols : int
    ; symbol_set : Smtml.Symbol.t list
    ; pc : Symbolic_value.vbool list
    ; memories : memories
    ; tables : Symbolic_table.collection
    ; globals : Symbolic_global.collection
        (** Breadcrumbs represent the list of choices that were made so far.
            They identify one given symbolic execution trace. *)
    ; breadcrumbs : int32 list
    }

  let symbols t = t.symbols

  let pc t = t.pc

  let memories t = t.memories

  let tables t = t.tables

  let globals t = t.globals

  let breadcrumbs t = t.breadcrumbs

  let symbols_set t = t.symbol_set

  let add_symbol t s = { t with symbol_set = s :: t.symbol_set }

  let add_pc t c = { t with pc = c :: t.pc }

  let add_breadcrumb t crumb = { t with breadcrumbs = crumb :: t.breadcrumbs }

  let incr_symbols t = { t with symbols = succ t.symbols }

  let create () =
    { symbols = 0
    ; symbol_set = []
    ; pc = []
    ; memories = M.init ()
    ; tables = Symbolic_table.init ()
    ; globals = Symbolic_global.init ()
    ; breadcrumbs = []
    }

  let clone { symbols; symbol_set; pc; memories; tables; globals; breadcrumbs }
      =
    let memories = M.clone memories in
    let tables = Symbolic_table.clone tables in
    let globals = Symbolic_global.clone globals in
    { symbols; symbol_set; pc; memories; tables; globals; breadcrumbs }
end

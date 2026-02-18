(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Collection : sig
  type 'a t

  val empty : 'a t

  val find : 'a t -> env_id:int -> id:int -> 'a option

  val replace : 'a t -> env_id:int -> id:int -> 'a -> 'a t
end

type t = private
  { num_symbols : int
  ; symbol_scopes : Symbol_scope.t
  ; pc : Symex.Path_condition.t
  ; memories : Symbolic_memory0.t Collection.t
  ; tables : Symbolic_table0.t Collection.t
  ; globals : Symbolic_global0.t Collection.t
      (** Breadcrumbs represent the list of choices that were made so far. They
          identify one given symbolic execution trace. *)
  ; breadcrumbs : int list
  ; depth : int
  ; labels : (int * string) list
  ; bench_stats : Benchmark.stats
  ; priority : Prio.metrics
  }

val init : unit -> t

val add_already_checked_to_pc : t -> Symbolic_boolean.t -> t

val add_breadcrumb : t -> int -> t

val add_symbol : t -> Smtml.Symbol.t -> t

val add_label : t -> int * string -> t

val replace_memory : Symbolic_memory0.t -> t -> t

val replace_table : Symbolic_table0.t -> t -> t

val replace_global : Symbolic_global0.t -> t -> t

val open_scope : t -> string -> t

val close_scope : t -> t

val incr_path_count : t -> unit

val incr_num_symbols : t -> t

val set_priority : Prio.metrics -> t -> t

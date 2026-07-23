(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type S0 = sig
  type 'a t

  type memory

  type i32

  type i64

  type f32

  type f64

  type v128

  (* TODO: change for invisible_i32? *)
  val symbol_invisible_bool : unit -> i32 t

  val symbol_i32 : unit -> i32 t

  val symbol_i64 : unit -> i64 t

  val symbol_f32 : unit -> f32 t

  val symbol_f64 : unit -> f64 t

  val symbol_v128 : unit -> v128 t

  val symbol_range : i32 -> i32 -> i32 t

  val assume : i32 -> unit t

  val assert' : i32 -> unit t

  val abort : unit -> unit t

  val alloc : memory -> i32 -> i32 -> i32 t

  val free : memory -> i32 -> i32 t

  val exit : i32 -> unit t

  val in_replay_mode : unit -> i32 t

  val print_char : i32 -> unit t

  val cov_label_is_covered : i32 -> i32 t

  val cov_label_set : memory -> i32 -> i32 -> unit t

  val open_scope_null_terminated : memory -> i32 -> unit t

  val open_scope_of_length : memory -> i32 -> i32 -> unit t

  val close_scope : unit -> unit t
end

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type S0 = sig
  type 'a t

  type memory

  module Value : Value_intf.T

  (* TODO: change for invisible_i32? *)
  val symbol_invisible_bool : unit -> Value.i32 t

  val symbol_i32 : unit -> Value.i32 t

  val symbol_i64 : unit -> Value.i64 t

  val symbol_f32 : unit -> Value.f32 t

  val symbol_f64 : unit -> Value.f64 t

  val symbol_v128 : unit -> Value.v128 t

  val symbol_range : Value.i32 -> Value.i32 -> Value.i32 t

  val assume : Value.i32 -> unit t

  val assert' : Value.i32 -> unit t

  val abort : unit -> unit t

  val alloc : memory -> Value.i32 -> Value.i32 -> Value.i32 t

  val free : memory -> Value.i32 -> Value.i32 t

  val exit : Value.i32 -> unit t

  val in_replay_mode : unit -> Value.i32 t

  val print_char : Value.i32 -> unit t

  val cov_label_is_covered : Value.i32 -> Value.i32 t

  val cov_label_set : memory -> Value.i32 -> Value.i32 -> unit t

  val open_scope : memory -> Value.i32 -> unit t

  val close_scope : unit -> unit t
end

module type S = sig
  type extern_func

  val symbolic_extern_module : extern_func Extern.Module.t
end

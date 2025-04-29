(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type S0 = sig
  type 'a t

  type memory

  module Value : sig
    type int32

    type int64

    type float32

    type float64
  end

  val symbol_i8 : unit -> Value.int32 t

  val symbol_char : unit -> Value.int32 t

  val symbol_i32 : unit -> Value.int32 t

  val symbol_i64 : unit -> Value.int64 t

  val symbol_f32 : unit -> Value.float32 t

  val symbol_f64 : unit -> Value.float64 t

  val symbol_invisible_bool : unit -> Value.int32 t

  val symbol_bool : unit -> Value.int32 t

  val symbol_range : Value.int32 -> Value.int32 -> Value.int32 t

  val assume : Value.int32 -> unit t

  val assert' : Value.int32 -> unit t

  val abort : unit -> unit t

  val alloc : memory -> Value.int32 -> Value.int32 -> Value.int32 t

  val free : memory -> Value.int32 -> Value.int32 t

  val exit : Value.int32 -> unit t

  val in_replay_mode : unit -> Value.int32 t

  val print_char : Value.int32 -> unit t

  val cov_label_is_covered : Value.int32 -> Value.int32 t

  val cov_label_set : memory -> Value.int32 -> Value.int32 -> unit t
end

module type S = sig
  type extern_func

  val symbolic_extern_module : extern_func Link.extern_module

  val summaries_extern_module : extern_func Link.extern_module
end

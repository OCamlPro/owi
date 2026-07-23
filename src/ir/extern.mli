(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Func : sig
  module Make (Value : sig
    type i32

    type i64

    type f32

    type f64

    type v128
  end) (M : sig
    (** The monad type *)
    type 'a t
  end) (Memory : sig
    (** The memory type *)
    type t
  end) : sig
    val fresh : unit -> int

    include
      Extern_intf.T
        with type i32 := Value.i32
         and type i64 := Value.i64
         and type f32 := Value.f32
         and type f64 := Value.f64
         and type v128 := Value.v128
         and type 'a m := 'a M.t
         and type memory := Memory.t
  end
end

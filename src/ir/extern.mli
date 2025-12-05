(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Func : sig
  module type T = sig
    type i32

    type i64

    type f32

    type f64

    type v128

    type 'a m

    type memory

    type _ telt = private
      | I32 : i32 telt
      | I64 : i64 telt
      | F32 : f32 telt
      | F64 : f64 telt
      | V128 : v128 telt
      | Externref : 'a Type.Id.t -> 'a telt

    type (_, _) atype = private
      | Mem : int * ('b, 'r) atype -> (memory -> 'b, 'r) atype
      | UArg : ('b, 'r) atype -> (unit -> 'b, 'r) atype
      | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
      | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
      | Res : ('r, 'r) atype

    type _ rtype = private
      | R0 : unit rtype
      | R1 : 'a telt -> 'a rtype
      | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
      | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
      | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

    type _ func_type = private
      | Func : ('f, 'r m) atype * 'r rtype -> 'f func_type

    type extern_func = Extern_func : 'a func_type * 'a -> extern_func

    val extern_type : extern_func -> Text.func_type

    module Syntax : sig
      type l

      type lr

      type elt

      type mem

      type (_, _, _) t

      val i32 : (lr, elt, i32) t

      val i64 : (lr, elt, i64) t

      val f32 : (lr, elt, f32) t

      val f64 : (lr, elt, f64) t

      val v128 : (lr, elt, v128) t

      val externref : 'a Type.Id.t -> (lr, elt, 'a) t

      val unit : (lr, unit, unit) t

      val memory : int -> (l, mem, memory) t

      val label : string -> (lr, elt, 'a) t -> (l, string * elt, 'a) t

      val ( ^-> ) : ('r, 'k, 'a) t -> 'b func_type -> ('a -> 'b) func_type

      val ( ^->. ) : ('r, 'k, 'a) t -> (lr, 'kk, 'b) t -> ('a -> 'b m) func_type

      val ( ^->.. ) :
           ('ll, 'k, 'a) t
        -> (lr, elt, 'b1) t * (lr, elt, 'b2) t
        -> ('a -> ('b1 * 'b2) m) func_type

      val ( ^->... ) :
           ('ll, 'k, 'a) t
        -> (lr, elt, 'b1) t * (lr, elt, 'b2) t * (lr, elt, 'b3) t
        -> ('a -> ('b1 * 'b2 * 'b3) m) func_type

      val ( ^->.... ) :
           ('ll, 'k, 'a) t
        -> (lr, elt, 'b1) t
           * (lr, elt, 'b2) t
           * (lr, elt, 'b3) t
           * (lr, elt, 'b4) t
        -> ('a -> ('b1 * 'b2 * 'b3 * 'b4) m) func_type
    end
  end

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
      T
        with type i32 := Value.i32
         and type i64 := Value.i64
         and type f32 := Value.f32
         and type f64 := Value.f64
         and type v128 := Value.v128
         and type 'a m := 'a M.t
         and type memory := Memory.t
  end
end

module Module : sig
  type 'f t =
    { functions : (string * 'f) list
    ; func_type : 'f -> Text.func_type
    }
end

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

module type Value_types = sig
  type int32

  type int64

  type float32

  type float64

  type vbool
end

module type Monad_type = sig
  type 'a t
end

module type T_Extern_func = sig
  type int32

  type int64

  type float32

  type float64

  type 'a m

  type _ telt =
    | I32 : int32 telt
    | I64 : int64 telt
    | F32 : float32 telt
    | F64 : float64 telt
    | Externref : 'a Type.Id.t -> 'a telt

  type _ rtype =
    | R0 : unit rtype
    | R1 : 'a telt -> 'a rtype
    | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
    | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
    | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

  type (_, _) atype =
    | UArg : ('b, 'r) atype -> (unit -> 'b, 'r) atype
    | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | Res : ('r, 'r) atype

  type _ func_type = Func : ('f, 'r m) atype * 'r rtype -> 'f func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func

  (* val extern_type : _ func_type -> Simplified.func_type *)
  val extern_type : extern_func -> binary Types.func_type
end

type t =
  | WASM of int * binary func * Env_id.t
  | Extern of Func_id.t

module type T = sig
  include T_Extern_func

  type nonrec t = t

  (* val typ : ('env, extern_func) t -> Simplified.func_type *)

  val wasm : binary func -> Env_id.t -> t
end

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

module type Value_types = sig
  type int32

  type int64

  type float32

  type float64

  type bool
end

module type Monad_type = sig
  type 'a t
end

module type Memory_type = sig
  type t
end

module type T_Extern_func = sig
  type int32

  type int64

  type float32

  type float64

  type 'a m

  type memory

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
    | Mem : ('b, 'r) atype -> (memory -> 'b, 'r) atype
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

module Make_extern_func
    (V : Value_types)
    (M : Monad_type)
    (Memory : Memory_type) : sig
  type nonrec t = t

  val fresh : unit -> int

  val wasm : Types.binary Types.func -> Env_id.t -> t

  include
    T_Extern_func
      with type int32 := V.int32
       and type int64 := V.int64
       and type float32 := V.float32
       and type float64 := V.float64
       and type 'a m := 'a M.t
       and type memory := Memory.t
end = struct
  type 'a m = 'a M.t

  type memory = Memory.t

  type _ telt =
    | I32 : V.int32 telt
    | I64 : V.int64 telt
    | F32 : V.float32 telt
    | F64 : V.float64 telt
    | Externref : 'a Type.Id.t -> 'a telt

  type _ rtype =
    | R0 : unit rtype
    | R1 : 'a telt -> 'a rtype
    | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
    | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
    | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

  type (_, _) atype =
    | Mem : ('b, 'r) atype -> (memory -> 'b, 'r) atype
    | UArg : ('b, 'r) atype -> (unit -> 'b, 'r) atype
    | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | Res : ('r, 'r) atype

  type _ func_type = Func : ('f, 'r m) atype * 'r rtype -> 'f func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func

  let elt_type (type t) (e : t telt) : binary val_type =
    match e with
    | I32 -> Num_type I32
    | I64 -> Num_type I64
    | F32 -> Num_type F32
    | F64 -> Num_type F64
    | Externref _ -> Ref_type (Null, Extern_ht)

  let res_type (type t) (r : t rtype) : binary result_type =
    match r with
    | R0 -> []
    | R1 a -> [ elt_type a ]
    | R2 (a, b) -> [ elt_type a; elt_type b ]
    | R3 (a, b, c) -> [ elt_type a; elt_type b; elt_type c ]
    | R4 (a, b, c, d) -> [ elt_type a; elt_type b; elt_type c; elt_type d ]

  let rec arg_type : type t r. (t, r) atype -> binary param_type = function
    | Mem tl -> arg_type tl
    | UArg tl -> arg_type tl
    | Arg (hd, tl) -> (None, elt_type hd) :: arg_type tl
    | NArg (name, hd, tl) -> (Some name, elt_type hd) :: arg_type tl
    | Res -> []

  let extern_type (Extern_func (Func (arg, res), _)) : binary Types.func_type =
    (arg_type arg, res_type res)

  type nonrec t = t

  let fresh =
    let r = ref ~-1 in
    fun () ->
      incr r;
      !r

  let wasm func env : t = WASM (fresh (), func, env)
end

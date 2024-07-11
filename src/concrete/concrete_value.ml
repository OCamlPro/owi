(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types
open Fmt

module Make_extern_func
    (V : Func_intf.Value_types)
    (M : Func_intf.Monad_type)
    (Memory : Func_intf.Memory_type) =
struct
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

  (* let extern_type (Func (arg, res)) : Simplified.func_type = *)
  (*   (arg_type arg, res_type res) *)

  let extern_type (Extern_func (Func (arg, res), _)) : binary Types.func_type =
    (arg_type arg, res_type res)

  type t = Func_intf.t

  let fresh =
    let r = ref ~-1 in
    fun () ->
      incr r;
      !r

  let wasm func env : t = WASM (fresh (), func, env)

  (* let typ = function *)
  (*   | Func_intf.WASM (_, func, _env) -> func.type_f *)
  (*   | Extern (Extern_func (t, _f)) -> extern_type t *)
end

module Func = struct
  include
    Make_extern_func
      (struct
        type int32 = Int32.t

        type int64 = Int64.t

        type float32 = Float32.t

        type float64 = Float64.t

        type vbool = bool
      end)
      (struct
        type 'a t = 'a
      end)
      (Concrete_memory)
end

type externref = E : 'a Type.Id.t * 'a -> externref

let cast_ref (type r) (E (rty, r) : externref) (ty : r Type.Id.t) : r option =
  match Type.Id.provably_equal rty ty with None -> None | Some Equal -> Some r

type ref_value =
  | Externref of externref option
  | Funcref of Func_intf.t option
  | Arrayref of unit Array.t option

let pp_ref_value fmt = function
  | Externref _ -> pf fmt "externref"
  | Funcref _ -> pf fmt "funcref"
  | Arrayref _ -> pf fmt "array"

type t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | Ref of ref_value

(* TODO: make a new kind of instr for this *)
let of_instr (i : binary instr) : t =
  match i with
  | I32_const c -> I32 c
  | I64_const c -> I64 c
  | F32_const c -> F32 c
  | F64_const c -> F64 c
  | _ -> assert false

let to_instr = function
  | I32 c -> I32_const c
  | I64 c -> I64_const c
  | F32 c -> F32_const c
  | F64 c -> F64_const c
  | Ref _ -> assert false

let pp fmt = function
  | I32 i -> pf fmt "i32.const %ld" i
  | I64 i -> pf fmt "i64.const %Ld" i
  | F32 f -> pf fmt "f32.const %a" Float32.pp f
  | F64 f -> pf fmt "f64.const %a" Float64.pp f
  | Ref r -> pp_ref_value fmt r

let ref_null' = function
  | Func_ht -> Funcref None
  | Extern_ht -> Externref None
  | Array_ht -> Arrayref None
  | Any_ht | None_ht | Eq_ht | I31_ht | Struct_ht | No_func_ht | No_extern_ht
  | Def_ht _ ->
    assert false

let ref_null typ = Ref (ref_null' typ)

let ref_func (f : Func.t) : t = Ref (Funcref (Some f))

let ref_externref (type x) (t : x Type.Id.t) (v : x) : t =
  Ref (Externref (Some (E (t, v))))

let ref_is_null = function
  | Funcref None | Externref None | Arrayref None -> true
  | Funcref (Some _) | Externref (Some _) | Arrayref (Some _) -> false

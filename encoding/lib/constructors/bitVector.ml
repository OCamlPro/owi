open Core
open Expression
open Types

exception Error of string

let mk_val (i : int) (t : num_type) : expr =
  match t with
  | `I32Type -> Val (Num (I32 (Int32.of_int_trunc i)))
  | `I64Type -> Val (Num (I64 (Int64.of_int i)))
  | _ -> raise (Error ("mk_val: invalid type '" ^ string_of_num_type t ^ "'"))

let mk_not (e : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Not
    | `I64Type -> I64 I64.Not
    | _ -> raise (Error ("mk_not: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Unop (op, e)

let mk_clz (e : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Clz
    | `I64Type -> I64 I64.Clz
    | _ -> raise (Error ("mk_clz: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Unop (op, e)

let mk_add (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Add
    | `I64Type -> I64 I64.Add
    | _ -> raise (Error ("mk_add: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_sub (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Sub
    | `I64Type -> I64 I64.Sub
    | _ -> raise (Error ("mk_sub: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_mul (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Mul
    | `I64Type -> I64 I64.Mul
    | _ -> raise (Error ("mk_mul: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_div_u (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.DivU
    | `I64Type -> I64 I64.DivU
    | _ ->
        raise (Error ("mk_div_u: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_div_s (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.DivS
    | `I64Type -> I64 I64.DivS
    | _ ->
        raise (Error ("mk_div_s: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_rem_u (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.RemU
    | `I64Type -> I64 I64.RemU
    | _ ->
        raise (Error ("mk_rem_u: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_rem_s (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.RemS
    | `I64Type -> I64 I64.RemS
    | _ ->
        raise (Error ("mk_rem_s: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_shl (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Shl
    | `I64Type -> I64 I64.Shl
    | _ -> raise (Error ("mk_shl: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_shr_u (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.ShrU
    | `I64Type -> I64 I64.ShrU
    | _ ->
        raise (Error ("mk_shr_u: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_shr_s (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.ShrS
    | `I64Type -> I64 I64.ShrS
    | _ ->
        raise (Error ("mk_shr_s: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_and (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.And
    | `I64Type -> I64 I64.And
    | _ -> raise (Error ("mk_and: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_or (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Or
    | `I64Type -> I64 I64.Or
    | _ -> raise (Error ("mk_or: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_xor (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Xor
    | `I64Type -> I64 I64.Xor
    | _ -> raise (Error ("mk_xor: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_eq (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Eq
    | `I64Type -> I64 I64.Eq
    | _ -> raise (Error ("mk_eq: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_ne (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.Ne
    | `I64Type -> I64 I64.Ne
    | _ -> raise (Error ("mk_ne: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_lt_u (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.LtU
    | `I64Type -> I64 I64.LtU
    | _ ->
        raise (Error ("mk_lt_u: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_lt_s (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.LtS
    | `I64Type -> I64 I64.LtS
    | _ ->
        raise (Error ("mk_lt_s: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_le_u (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.LeU
    | `I64Type -> I64 I64.LeU
    | _ ->
        raise (Error ("mk_le_u: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_le_s (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.LeS
    | `I64Type -> I64 I64.LeS
    | _ ->
        raise (Error ("mk_le_s: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_gt_u (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.GtU
    | `I64Type -> I64 I64.GtU
    | _ ->
        raise (Error ("mk_gt_u: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_gt_s (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.GtS
    | `I64Type -> I64 I64.GtS
    | _ ->
        raise (Error ("mk_gt_s: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_ge_u (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.GeU
    | `I64Type -> I64 I64.GeU
    | _ ->
        raise (Error ("mk_ge_u: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_ge_s (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `I32Type -> I32 I32.GeS
    | `I64Type -> I64 I64.GeS
    | _ ->
        raise (Error ("mk_ge_s: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

open Core
open Expression
open Types

exception Error of string

let mk_val (f : float) (t : num_type) : expr =
  match t with
  | `F32Type -> Val (Num (F32 (Int32.bits_of_float f)))
  | `F64Type -> Val (Num (F64 (Int64.bits_of_float f)))
  | _ -> raise (Error ("mk_val: invalid type '" ^ string_of_num_type t ^ "'"))

let mk_neg (e : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Neg
    | `F64Type -> F64 F64.Neg
    | _ -> raise (Error ("mk_neg: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Unop (op, e)

let mk_abs (e : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Abs
    | `F64Type -> F64 F64.Abs
    | _ -> raise (Error ("mk_abs: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Unop (op, e)

let mk_sqrt (e : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Sqrt
    | `F64Type -> F64 F64.Sqrt
    | _ ->
        raise (Error ("mk_sqrt: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Unop (op, e)

let mk_nearest (e : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Nearest
    | `F64Type -> F64 F64.Nearest
    | _ ->
        raise
          (Error ("mk_nearest: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Unop (op, e)

let mk_is_nan (e : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.IsNan
    | `F64Type -> F64 F64.IsNan
    | _ ->
        raise (Error ("mk_is_nan: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Unop (op, e)

let mk_add (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Add
    | `F64Type -> F64 F64.Add
    | _ -> raise (Error ("mk_add: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_sub (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Sub
    | `F64Type -> F64 F64.Sub
    | _ -> raise (Error ("mk_sub: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_mul (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Mul
    | `F64Type -> F64 F64.Mul
    | _ -> raise (Error ("mk_mul: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_div (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Div
    | `F64Type -> F64 F64.Div
    | _ -> raise (Error ("mk_div: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_min (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Min
    | `F64Type -> F64 F64.Min
    | _ -> raise (Error ("mk_min: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_max (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Max
    | `F64Type -> F64 F64.Max
    | _ -> raise (Error ("mk_max: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_rem (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Rem
    | `F64Type -> F64 F64.Rem
    | _ -> raise (Error ("mk_max: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Binop (op, e1, e2)

let mk_eq (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Eq
    | `F64Type -> F64 F64.Eq
    | _ -> raise (Error ("mk_eq: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_ne (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Ne
    | `F64Type -> F64 F64.Ne
    | _ -> raise (Error ("mk_ne: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_lt (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Lt
    | `F64Type -> F64 F64.Lt
    | _ -> raise (Error ("mk_lt: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_le (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Le
    | `F64Type -> F64 F64.Le
    | _ -> raise (Error ("mk_le: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_gt (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Gt
    | `F64Type -> F64 F64.Gt
    | _ -> raise (Error ("mk_gt: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

let mk_ge (e1 : expr) (e2 : expr) (t : num_type) : expr =
  let op =
    match t with
    | `F32Type -> F32 F32.Ge
    | `F64Type -> F64 F64.Ge
    | _ -> raise (Error ("mk_ge: invalid type '" ^ string_of_num_type t ^ "'"))
  in
  Relop (op, e1, e2)

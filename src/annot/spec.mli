(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Types

type nonrec binpred =
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Neq

type nonrec unconnect = Not

type nonrec binconnect =
  | And
  | Or
  | Imply
  | Equiv

type nonrec binder =
  | Forall
  | Exists

type nonrec binder_type = num_type

type nonrec unop = Neg

type nonrec binop =
  | Plus
  | Minus
  | Mult
  | Div

type 'a term =
  | Int32 : Int32.t -> 'a term
  | Int64 : Int64.t -> 'a term
  | Float32 : Float32.t -> 'a term
  | Float64 : Float64.t -> 'a term
  | Var : text indice -> text term
  | ParamVar : 'a indice -> 'a term
  | GlobalVar : 'a indice -> 'a term
  | BinderVar : 'a indice -> 'a term
  | UnOp : unop * 'a term -> 'a term
  | BinOp : binop * 'a term * 'a term -> 'a term
  | Result : int option -> 'a term
  | Memory : 'a term -> 'a term

type 'a prop =
  | Const : bool -> 'a prop
  | BinPred : binpred * 'a term * 'a term -> 'a prop
  | UnConnect : unconnect * 'a prop -> 'a prop
  | BinConnect : binconnect * 'a prop * 'a prop -> 'a prop
  | Binder : binder * binder_type * string option * 'a prop -> 'a prop

val pp_binpred : Format.formatter -> binpred -> unit

val pp_unconnect : Format.formatter -> unconnect -> unit

val pp_binconnect : Format.formatter -> binconnect -> unit

val pp_binder : Format.formatter -> binder -> unit

val pp_binder_type : Format.formatter -> binder_type -> unit

val pp_unop : Format.formatter -> unop -> unit

val pp_binop : Format.formatter -> binop -> unit

val pp_term : Format.formatter -> 'a term -> unit

val pp_prop : Format.formatter -> 'a prop -> unit

val parse_indice : string -> text indice Result.t

val parse_prop : Sexp.t -> text prop Result.t

val parse_term : Sexp.t -> text term Result.t

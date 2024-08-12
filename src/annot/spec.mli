open Types
open Fmt

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
  | Int32 : int32 -> 'a term
  | Var : text indice -> text term
  | GlobalVar : 'a indice -> 'a term
  | BinderVar : 'a indice -> 'a term
  | UnOp : unop * 'a term -> 'a term
  | BinOp : binop * 'a term * 'a term -> 'a term
  | Result : 'a term

type 'a prop =
  | Const : bool -> 'a prop
  | BinPred : binpred * 'a term * 'a term -> 'a prop
  | UnConnect : unconnect * 'a prop -> 'a prop
  | BinConnect : binconnect * 'a prop * 'a prop -> 'a prop
  | Binder : binder * binder_type * string option * 'a prop -> 'a prop

val pp_bool : formatter -> bool -> unit

val pp_binpred : formatter -> binpred -> unit

val pp_unconnect : formatter -> unconnect -> unit

val pp_binconnect : formatter -> binconnect -> unit

val pp_binder : formatter -> binder -> unit

val pp_binder_type : formatter -> binder_type -> unit

val pp_prop : formatter -> 'a prop -> unit

val pp_term : formatter -> 'a term -> unit

val parse_indice : string -> text indice Result.t

val parse_prop : Sexp.t -> text prop Result.t

val parse_term : Sexp.t -> text term Result.t

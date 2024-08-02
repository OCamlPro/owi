open Types
open Fmt

(*
  text:
    (local index)
    (global index)
    (binder index)
    string_id

  binary:
    (local index)
    (global index)
    (binder index)
*)

type nonrec binpred =
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Neq

type nonrec unconnect = Neg

type nonrec binconnect =
  | And
  | Or
  | Imply
  | Equiv

type nonrec binder =
  | Forall
  | Exists

type nonrec binder_type = num_type

type 'a prop =
  | Const : bool -> 'a prop
  | BinPred : binpred * 'a term * 'a term -> 'a prop
  | UnConnect : unconnect * 'a prop -> 'a prop
  | BinConnect : binconnect * 'a prop * 'a prop -> 'a prop
  | Binder : binder * binder_type * string option * 'a prop -> 'a prop

and 'a term =
  | Int32 : int32 -> 'a term
  | Var : text indice -> text term
  | GlobalVar : 'a indice -> 'a term
  | BinderVar : 'a indice -> 'a term
  | Result : 'a term

let pp_bool fmt = function true -> pf fmt "true" | false -> pf fmt "false"

let pp_binpred fmt = function
  | Ge -> pf fmt ">="
  | Gt -> pf fmt ">"
  | Le -> pf fmt "<="
  | Lt -> pf fmt "<"
  | Eq -> pf fmt "="
  | Neq -> pf fmt "!="

let pp_unconnect fmt = function Neg -> pf fmt "!"

let pp_binconnect fmt = function
  | And -> pf fmt "&&"
  | Or -> pf fmt "||"
  | Imply -> pf fmt "==>"
  | Equiv -> pf fmt "<==>"

let pp_binder fmt = function
  | Forall -> pf fmt {|\forall|}
  | Exists -> pf fmt {|\exists|}

let pp_binder_type = pp_num_type

let rec pp_prop fmt = function
  | Const bool -> pf fmt {|"\%a"|} pp_bool bool
  | BinPred (b, tm1, tm2) ->
    pf fmt "@[<hv 2>%a@ %a@ %a@]" pp_term tm1 pp_binpred b pp_term tm2
  | UnConnect (u, pr1) -> pf fmt "@[<hv 2>%a@ %a@]" pp_unconnect u pp_prop pr1
  | BinConnect (b, pr1, pr2) ->
    pf fmt "@[<hv 2>%a@ %a@ %a@]" pp_prop pr1 pp_binconnect b pp_prop pr2
  | Binder (b, bt, id_opt, pr1) -> (
    match id_opt with
    | Some id ->
      pf fmt "@[<hv 2>%a@ %a@ %a, %a@]" pp_binder b pp_binder_type bt pp_id id
        pp_prop pr1
    | None ->
      pf fmt "@[<hv 2>%a@ %a@, %a@]" pp_binder b pp_binder_type bt pp_prop pr1 )

and pp_term (type e) fmt (tm : e term) =
  match tm with
  | Int32 i -> pf fmt "%i" (Int32.to_int i)
  | Var ind -> pf fmt "%a" pp_indice ind
  | GlobalVar ind -> pf fmt "global.%a" pp_indice ind
  | BinderVar ind -> pf fmt "binder.%a" pp_indice ind
  | Result -> pf fmt {|\result|}

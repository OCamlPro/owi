open Types

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

type 'a prop =
  | Const of bool
  | BinPred of binpred * 'a term * 'a term
  | UnConnect of unconnect * 'a prop
  | BinConnect of binconnect * 'a prop * 'a prop

and 'a term =
  | Int of int
  | Global of 'a indice
  | Result

type 'a t =
  { func : 'a indice
  ; preconditions : 'a prop list
  ; postconditions : 'a prop list
  }

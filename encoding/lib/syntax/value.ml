open Core
open Types

type t =
  | Int of Int.t
  | Real of Float.t
  | Bool of Bool.t
  | Num of Num.t
  | Str of String.t
[@@deriving compare, sexp_of, hash]

let equal (v1 : t) (v2 : t) : Bool.t =
  match (v1, v2) with
  | Int x1, Int x2 -> Int.equal x1 x2
  | Real x1, Real x2 -> Float.equal x1 x2
  | Bool x1, Bool x2 -> Bool.equal x1 x2
  | Num x1, Num x2 -> Num.(x1 = x2)
  | Str x1, Str x2 -> String.equal x1 x2
  | _ -> false

let type_of (v : t) : expr_type =
  match v with
  | Int _ -> `IntType
  | Real _ -> `RealType
  | Bool _ -> `BoolType
  | Num n -> Num.type_of n
  | Str _ -> `StrType

let to_string (v : t) : String.t =
  match v with
  | Int x -> Int.to_string x
  | Real x -> Float.to_string x
  | Bool x -> Bool.to_string x
  | Num x -> Num.to_string x
  | Str x -> "\"" ^ x ^ "\""

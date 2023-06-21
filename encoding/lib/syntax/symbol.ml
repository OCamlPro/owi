open Core
open Types

type t = { sort : expr_type; name : String.t }
[@@deriving compare, sexp_of, hash]

let mk_symbol (sort : expr_type) (name : string) = { sort; name }

let equal (s1 : t) (s2 : t) : bool =
  Poly.(s1.sort = s2.sort) && String.equal s1.name s2.name

let rename (s : t) (name : String.t) = { s with name }
let type_of (s : t) : expr_type = s.sort
let to_string (s : t) : string = s.name

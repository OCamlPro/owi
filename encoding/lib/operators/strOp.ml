type binop = Nth | Concat [@@deriving compare, sexp_of, hash]
type unop = Len | Trim [@@deriving compare, sexp_of, hash]
type relop = Eq | Ne [@@deriving compare, sexp_of, hash]
type triop = SubStr [@@deriving compare, sexp_of, hash]
type cvtop [@@deriving compare, sexp_of, hash]

let neg_relop (op : relop) : relop = match op with Eq -> Ne | Ne -> Eq

let string_of_binop (op : binop) : string =
  match op with Nth -> "nth" | Concat -> "++"

let string_of_unop (op : unop) : string =
  match op with Len -> "len" | Trim -> "trim"

let string_of_triop (op : triop) : string = match op with SubStr -> "sub"

let string_of_relop (op : relop) : string =
  match op with Eq -> "eq" | Ne -> "ne"

let string_of_cvtop (_ : cvtop) : string = assert false

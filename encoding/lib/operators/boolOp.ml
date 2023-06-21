type binop = And | Or | Xor [@@deriving compare, sexp_of, hash]
type unop = Not [@@deriving compare, sexp_of, hash]
type relop = Eq | Ne [@@deriving compare, sexp_of, hash]
type triop = ITE [@@deriving compare, sexp_of, hash]
type cvtop [@@deriving compare, sexp_of, hash]

let neg_relop (op : relop) : relop = match op with Eq -> Ne | Ne -> Eq
let string_of_unop (op : unop) : string = match op with Not -> "not"

let string_of_binop (op : binop) : string =
  match op with And -> "and" | Or -> "or" | Xor -> "xor"

let string_of_relop (op : relop) : string =
  match op with Eq -> "eq" | Ne -> "ne"

let string_of_cvtop (_ : cvtop) : string = assert false
let string_of_triop (op : triop) : string = match op with ITE -> "ite"

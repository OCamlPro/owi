type binop =
  | Add
  | Mul
  | Div
  | Rem
  | And
  | Sub
  | Shl
  | ShrA
  | ShrL
  | Or
  | Xor
  | Pow
[@@deriving compare, sexp_of, hash]

type unop = Neg [@@deriving compare, sexp_of, hash]
type relop = Eq | Lt | Le | Ne | Gt | Ge [@@deriving compare, sexp_of, hash]
type triop [@@deriving compare, sexp_of, hash]

type cvtop = ToString | OfString | ReinterpretReal
[@@deriving compare, sexp_of, hash]

let neg_relop (op : relop) : relop =
  match op with
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt

let string_of_binop (op : binop) : string =
  match op with
  | Add -> "add"
  | And -> "and"
  | Or -> "or"
  | Sub -> "sub"
  | Div -> "div"
  | Xor -> "xor"
  | Mul -> "mul"
  | Shl -> "shl"
  | ShrA -> "shr_a"
  | ShrL -> "shr_u"
  | Rem -> "rem"
  | Pow -> "pow"

let string_of_unop (op : unop) : string = match op with Neg -> "neg"

let string_of_relop (op : relop) : string =
  match op with
  | Eq -> "eq"
  | Ne -> "ne"
  | Lt -> "lt"
  | Gt -> "gt"
  | Le -> "le"
  | Ge -> "ge"

let string_of_cvtop (op : cvtop) : string =
  match op with
  | ToString -> "to_string"
  | OfString -> "of_string"
  | ReinterpretReal -> "reinterpret_real"

let string_of_triop (_ : triop) : string = assert false

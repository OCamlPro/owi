type binop = Add | Sub | Mul | Div | Min | Max | Rem (*  Falta: | CopySign *)
[@@deriving compare, sexp_of, hash]

type unop =
  | Neg
  | Abs
  | Sqrt
  | Nearest
  | IsNan (*  Falta: | Ceil | Floor | Trunc *)
[@@deriving compare, sexp_of, hash]

type relop = Eq | Ne | Lt | Le | Gt | Ge [@@deriving compare, sexp_of, hash]
type triop [@@deriving compare, sexp_of, hash]

type cvtop =
  | DemoteF64
  | ConvertSI32
  | ConvertUI32
  | ConvertSI64
  | ConvertUI64
  | ReinterpretInt
  | PromoteF32
  | ToString
  | OfString
[@@deriving compare, sexp_of, hash]

let neg_relop (op : relop) : relop =
  match op with
  | Eq -> Ne
  | Ne -> Eq
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt

let string_of_unop (op : unop) : string =
  match op with
  | Neg -> "neg"
  | Abs -> "abs"
  | Sqrt -> "sqrt"
  | Nearest -> "nearest"
  | IsNan -> "is_nan"

let string_of_binop (op : binop) : string =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Min -> "min"
  | Max -> "max"
  | Rem -> "rem"

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
  | DemoteF64 -> "demote_f64"
  | ConvertSI32 -> "convert_i32_s"
  | ConvertUI32 -> "convert_i32_u"
  | ConvertSI64 -> "convert_i64_s"
  | ConvertUI64 -> "convert_i64_u"
  | ReinterpretInt -> "reinterpret_int"
  | PromoteF32 -> "promote_f32"
  | ToString -> "to_string"
  | OfString -> "of_string"

let string_of_triop (_ : triop) : string = assert false

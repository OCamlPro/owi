type binop =
  | Add
  | Mul
  | DivU
  | RemU
  | ShrU
  | And
  | Sub
  | Shl
  | DivS
  | RemS
  | ShrS
  | Or
  | Xor
  | Rotl
  | Rotr
[@@deriving compare, sexp_of, hash]

type unop = Not | Clz (*  Falta:  Ctz | Popcnt *)
[@@deriving compare, sexp_of, hash]

type relop = Eq | LtU | LtS | LeU | LeS | Ne | GtU | GtS | GeU | GeS
[@@deriving compare, sexp_of, hash]

type triop [@@deriving compare, sexp_of, hash]

type cvtop =
  | TruncSF32
  | TruncUF32
  | TruncSF64
  | TruncUF64
  | ReinterpretFloat
  | WrapI64
  | ExtendSI32
  | ExtendUI32
[@@deriving compare, sexp_of, hash]

let neg_relop (op : relop) : relop =
  match op with
  | Eq -> Ne
  | Ne -> Eq
  | LtU -> GeU
  | LtS -> GeS
  | GtU -> LeU
  | GtS -> LeS
  | LeU -> GtU
  | LeS -> GtS
  | GeU -> LtU
  | GeS -> LtS

let string_of_unop (op : unop) : string =
  match op with Clz -> "clz" | Not -> "not"

let string_of_binop (op : binop) : string =
  match op with
  | Add -> "add"
  | Sub -> "sub"
  | DivS -> "div_s"
  | DivU -> "div_u"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Mul -> "mul"
  | Shl -> "shl"
  | ShrS -> "shr_s"
  | ShrU -> "shr_u"
  | RemS -> "rem_s"
  | RemU -> "rem_u"
  | Rotl -> "rotl"
  | Rotr -> "rotr"

let string_of_relop (op : relop) : string =
  match op with
  | Eq -> "eq"
  | Ne -> "ne"
  | LtU -> "lt_u"
  | LtS -> "lt_s"
  | GtU -> "gt_u"
  | GtS -> "gt_s"
  | LeU -> "le_u"
  | LeS -> "le_s"
  | GeU -> "ge_u"
  | GeS -> "ge_s"

let string_of_cvtop (op : cvtop) : string =
  match op with
  | WrapI64 -> "wrap_i64"
  | TruncSF32 -> "trunc_f32_s"
  | TruncUF32 -> "trunc_f32_u"
  | TruncSF64 -> "trunc_f64_s"
  | TruncUF64 -> "trunc_f64_u"
  | ReinterpretFloat -> "reinterpret_float"
  | ExtendSI32 -> "extend_i32_s"
  | ExtendUI32 -> "extend_i32_u"

let string_of_triop (_ : triop) : string = assert false

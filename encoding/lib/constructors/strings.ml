open Expression
open Types

let mk_val (s : String.t) : expr = Val (Str s)
let mk_len (s : expr) : expr = Unop (Str S.Len, s)
let mk_nth (s : expr) (i : expr) : expr = Binop (Str S.Nth, s, i)
let mk_concat (s1 : expr) (s2 : expr) : expr = Binop (Str S.Concat, s1, s2)
let mk_eq (s1 : expr) (s2 : expr) : expr = Relop (Str S.Eq, s1, s2)
let mk_ne (s1 : expr) (s2 : expr) : expr = Relop (Str S.Ne, s1, s2)
let mk_trim (s : expr) : expr = Unop (Str S.Trim, s)

let mk_substr (s : expr) ~(pos : expr) ~(len : expr) : expr =
  Triop (Str S.SubStr, s, pos, len)

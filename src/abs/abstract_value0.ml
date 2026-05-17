module Dom = Abstract_domain_intv

type i32 = Dom.binary

type i64 = Dom.binary

type boolean = Dom.binary

type t =
  | I32 of i32
  | I64 of i64

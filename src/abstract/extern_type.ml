type _ telt =
  | I32 : Abstract_value.i32 telt
  | I64 : Abstract_value.i64 telt
  | F32 : Abstract_value.f32 telt
  | F64 : Abstract_value.f64 telt
  | V128 : Abstract_value.v128 telt
  | Externref : 'a Type.Id.t -> 'a telt

type _ rtype =
  | R0 : unit rtype
  | R1 : 'a telt -> 'a rtype
  | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
  | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
  | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

type (_, _) atype =
  | Mem : int * ('b, 'r) atype -> ('memory -> 'b, 'r) atype
  | UArg : ('b, 'r) atype -> (unit -> 'b, 'r) atype
  | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
  | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
  | Res : ('r, 'r) atype

type 'a t = 'a

type _ func_type = Func : ('f, 'r t) atype * 'r rtype -> 'f func_type

type extern_func = Extern_func : 'a func_type * 'a -> extern_func

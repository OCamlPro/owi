module type T = sig
  type int32

  type int64

  type float32

  type float64

  type _ telt =
    | I32 : int32 telt
    | I64 : int64 telt
    | F32 : float32 telt
    | F64 : float64 telt
    | Externref : 'a Type_id.ty -> 'a telt

  type _ rtype =
    | R0 : unit rtype
    | R1 : 'a telt -> 'a rtype
    | R2 : 'a telt * 'b telt -> ('a * 'b) rtype
    | R3 : 'a telt * 'b telt * 'c telt -> ('a * 'b * 'c) rtype
    | R4 : 'a telt * 'b telt * 'c telt * 'd telt -> ('a * 'b * 'c * 'd) rtype

  type (_, _) atype =
    | Arg : 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | NArg : string * 'a telt * ('b, 'r) atype -> ('a -> 'b, 'r) atype
    | Res : ('r, 'r) atype

  type _ func_type = Func : ('f, 'r) atype * 'r rtype -> 'f func_type

  type extern_func = Extern_func : 'a func_type * 'a -> extern_func

  type 'a t =
    | WASM of int * Simplified.func * 'a
    | Extern of extern_func

  val typ : 'a t -> Simplified.func_type

  val wasm : Simplified.func -> 'a -> 'a t
end

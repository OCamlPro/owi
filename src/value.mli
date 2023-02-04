(** Module to define externref values in OCaml. You should look in the `example`
    directory to understand how to use this before reading the code... *)

type ('a, 'b) eq = Eq : ('a, 'a) eq

module Extern_ref : sig
  type 'a ty

  val fresh : string -> 'a ty

  val name : 'a ty -> string

  val eq : 'a ty -> 'b ty -> ('a, 'b) eq option
end

type externref = E : 'a Extern_ref.ty * 'a -> externref

module Func : sig
  type _ telt =
    | I32 : Int32.t telt
    | I64 : Int64.t telt
    | F32 : Float32.t telt
    | F64 : Float64.t telt
    | Externref : 'a Extern_ref.ty -> 'a telt

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
    | WASM of int * (int, Types.func_type) Types.func' * 'a
    | Extern of extern_func

  val typ : 'a t -> Types.func_type

  val wasm : (int, Types.func_type) Types.func' -> 'a -> 'a t
end

type 'a ref_value =
  | Externref of externref option
  | Funcref of 'a Func.t option

type 'a t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | Ref of 'a ref_value

val cast_ref : externref -> 'a Extern_ref.ty -> 'a option

val ref_null' : Types.heap_type -> 'a ref_value

val ref_null : Types.heap_type -> 'a t

val ref_func : 'a Func.t -> 'a t

val is_ref_null : 'a ref_value -> bool

val pp : Format.formatter -> 'a t -> unit

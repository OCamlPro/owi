(** Module to define externref values in OCaml. You should look in the `example`
    directory to understand how to use this before reading the code... *)

type externref = E : 'a Type_id.ty * 'a -> externref

module Make_extern_func (V : Func_intf.Value_types) (M : Func_intf.Monad_type) :
  Func_intf.T_Extern_func
    with type int32 := V.int32
     and type int64 := V.int64
     and type float32 := V.float32
     and type float64 := V.float64
     and type 'a m := 'a M.t

module Func :
  Func_intf.T
    with type int32 := Int32.t
     and type int64 := Int64.t
     and type float32 := Float32.t
     and type float64 := Float64.t
     and type 'a m := 'a

type ref_value =
  | Externref of externref option
  | Funcref of Func_intf.t option
  | Arrayref of unit array option

type t =
  | I32 of Int32.t
  | I64 of Int64.t
  | F32 of Float32.t
  | F64 of Float64.t
  | Ref of ref_value

val cast_ref : externref -> 'a Type_id.ty -> 'a option

val of_instr : Simplified.instr -> t

val to_instr : t -> Simplified.instr

val ref_null' : Simplified.heap_type -> ref_value

val ref_null : Simplified.heap_type -> t

val ref_func : Func.t -> t

val ref_is_null : ref_value -> bool

val pp : Format.formatter -> t -> unit

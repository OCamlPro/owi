open Types

type 'ext t

type t' = Env_id.t

type elem = { mutable value : Concrete_value.ref_value array }

type data = { mutable value : string }

type func := Func_intf.t

val get_memory : _ t -> int -> Concrete_memory.t

val get_func : _ t -> int -> func

val get_table : _ t -> int -> Concrete_table.t

val get_elem : _ t -> int -> elem

val get_data : _ t -> int -> data

val get_global : _ t -> int -> Concrete_global.t

val drop_elem : elem -> unit

val drop_data : data -> unit

val get_extern_func : 'ext t -> Func_id.t -> 'ext

val id : _ t -> Env_id.t

module Build : sig
  type t

  val empty : t

  val add_global : int -> Concrete_global.t -> t -> t

  val add_memory : int -> Concrete_memory.t -> t -> t

  val add_table : int -> Concrete_table.t -> t -> t

  val add_func : int -> func -> t -> t

  val add_data : int -> data -> t -> t

  val add_elem : int -> elem -> t -> t

  val get_const_global : t -> int -> Concrete_value.t Result.t

  val get_func : t -> int -> func Result.t
end

type extern_funcs = Concrete_value.Func.extern_func Func_id.collection

val freeze : t' -> Build.t -> 'ext Func_id.collection -> 'ext t

module type T = sig

  type extern_func

  type t

  type elem = { mutable value : Concrete_value.ref_value array }

  type data = { mutable value : string }

  val get_memory : t -> int -> Concrete_memory.t Result.t

  val get_func : t -> int -> func Result.t

  val get_table : t -> int -> Concrete_table.t Result.t

  val get_elem : t -> int -> elem Result.t

  val get_data : t -> int -> data Result.t

  val get_global : t -> int -> Concrete_global.t Result.t

  val drop_elem : elem -> unit

  val drop_data : data -> unit

  val get_extern_func : t -> Func_id.t -> Concrete_value.Func.extern_func

  val get_func_typ : t -> func -> simplified func_type

  val pp : Format.formatter -> t -> unit

  val freeze : Build.t -> extern_func Func_id.collection -> t
end

module type P = sig

  val const_i32 : Int32.t -> V.int32

  val const_i64 : Int64.t -> V.int64

  val const_f32 : Float32.t -> V.float32

  val const_f64 : Float64.t -> V.float64
end

(* module Make(P : P) : T with module V := P.V *)

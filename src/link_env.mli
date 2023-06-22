type t

type t' = Env_id.t

type elem = { mutable value : Value.ref_value array }

type data = { mutable value : string }

type func := Func_intf.t

val get_memory : t -> int -> Memory.t Result.t

val get_func : t -> int -> func Result.t

val get_table : t -> int -> Table.t Result.t

val get_elem : t -> int -> elem Result.t

val get_data : t -> int -> data Result.t

val get_global : t -> int -> Global.t Result.t

val drop_elem : elem -> unit

val drop_data : data -> unit

val get_extern_func : t -> Func_id.t -> Value.Func.extern_func

val get_func_typ : t -> func -> Simplified.func_type

val pp : Format.formatter -> t -> unit

module Build : sig
  type t

  val empty : t

  val add_global : int -> Global.t -> t -> t

  val add_memory : int -> Memory.t -> t -> t

  val add_table : int -> Table.t -> t -> t

  val add_func : int -> func -> t -> t

  val add_data : int -> data -> t -> t

  val add_elem : int -> elem -> t -> t

  val get_const_global : t -> int -> Value.t Result.t

  val get_func : t -> int -> func Result.t
end

type extern_funcs = Value.Func.extern_func Func_id.collection

val freeze : Build.t -> extern_funcs -> t

module type T = sig
  module V : Intf.V

  type extern_func

  type t

  type t' = t Lazy.t

  type elem = { mutable value : Value.ref_value array }

  type data = { mutable value : string }

  val get_memory : t -> int -> Memory.t Result.t

  val get_func : t -> int -> func Result.t

  val get_table : t -> int -> Table.t Result.t

  val get_elem : t -> int -> elem Result.t

  val get_data : t -> int -> data Result.t

  val get_global : t -> int -> Global.t Result.t

  val drop_elem : elem -> unit

  val drop_data : data -> unit

  val get_extern_func : t -> Func_id.t -> Value.Func.extern_func

  val get_func_typ : t -> func -> Simplified.func_type

  val pp : Format.formatter -> t -> unit

  val freeze : Build.t -> extern_func Func_id.collection -> t
end

module type P = sig
  module V : Intf.V

  val const_i32 : Int32.t -> V.int32

  val const_i64 : Int64.t -> V.int64

  val const_f32 : Float32.t -> V.float32

  val const_f64 : Float64.t -> V.float64
end

(* module Make(P : P) : T with module V := P.V *)

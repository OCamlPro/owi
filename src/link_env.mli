type t

type t' = t Lazy.t

type 'env elem = { mutable value : 'env Value.ref_value array }

type data = { mutable value : string }

type func := (t', Func_id.t) Func_intf.t

val get_memory : t -> int -> Memory.t Result.t

val get_func : t -> int -> func Result.t

val get_table : t -> int -> t' Table.t Result.t

val get_elem : t -> int -> t' elem Result.t

val get_data : t -> int -> data Result.t

val get_global : t -> int -> t' Global.t Result.t

val drop_elem : 'a elem -> unit

val drop_data : data -> unit

val get_extern_func : t -> Func_id.t -> Value.Func.extern_func

val get_func_typ : t -> func -> Simplified.func_type

val pp : Format.formatter -> t -> unit

module Build : sig
  type t

  val empty : t

  val add_global : int -> t' Global.t -> t -> t

  val add_memory : int -> Memory.t -> t -> t

  val add_table : int -> t' Table.t -> t -> t

  val add_func : int -> func -> t -> t

  val add_data : int -> data -> t -> t

  val add_elem : int -> t' elem -> t -> t

  val get_const_global : t -> int -> t' Value.t Result.t

  val get_func : t -> int -> func Result.t
end

type extern_funcs = Value.Func.extern_func Func_id.collection

val freeze : Build.t -> extern_funcs -> t

module StringMap : sig
  type 'a t

  val find : string -> 'a t -> 'a
end

module StringSet : sig
  type t
end

module Memory : sig
  type t

  val get_data : t -> bytes

  val get_limit_max : t -> int option

  val update_memory : t -> bytes -> unit
end

module Table : sig
  type 'env table = 'env Value.ref_value array

  type 'env t =
    { id : int
    ; label : string option
    ; limits : Types.limits
    ; type_ : Types.ref_type
    ; mutable data : 'env table
    }

  val update : 'a t -> 'a table -> unit
end

module Global : sig
  type 'env t =
    { mutable value : 'env Value.t
    ; label : string option
    ; mut : Types.mut
    ; typ : Types.val_type
    }
end

module Env : sig
  type t

  type t' = t lazy_t

  type 'env elem = { mutable value : 'env Value.ref_value array }

  type data = { mutable value : string }

  val get_memory : t -> int -> Memory.t

  val get_func : t -> int -> t' Value.func

  val get_table : t -> int -> t' Table.t

  val get_elem : t -> int -> t' elem

  val get_data : t -> int -> data

  val get_global : t -> int -> t' Global.t

  val drop_elem : 'a elem -> unit

  val drop_data : data -> unit
end

module IMap : sig
  type key

  type 'a t

  val find_opt : key -> 'a t -> 'a option
end

type module_to_run =
  { module_ : Simplify.result
  ; env : Env.t
  ; to_run : (Simplify.index, Types.func_type) Types.expr' list
  }

type exports =
  { globals : Env.t' Global.t StringMap.t
  ; memories : Memory.t StringMap.t
  ; tables : Env.t' Table.t StringMap.t
  ; functions : Env.t' Value.func StringMap.t
  ; defined_names : StringSet.t
  }

type state =
  { by_name : exports StringMap.t
  ; by_id : exports StringMap.t
  ; last : exports option
  }

val module_ : Simplify.result -> state -> (module_to_run * state, string) result

val empty_state : state

val register_module : state -> name:string -> id:string option -> state

type extern_module = { functions : (string * Value.extern_func) list }

val extern_module : string -> extern_module -> state -> state

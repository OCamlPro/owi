(** the types of simplified modules *)

(** the types of imported values *)
type 'a imp =
  { module_ : string
  ; name : string
  ; assigned_name : string option
  ; desc : 'a
  }

(** a value that is either local or imported *)
type ('a, 'b) runtime =
  | Local of 'a
  | Imported of 'b imp

(** int indexed values *)
type 'a indexed =
  { index : int
  ; value : 'a
  }

module StringMap : Map.S with type key = string

(** named values (fields) *)
module Named : sig
  type 'a t =
    { values : 'a indexed list
    ; named : int StringMap.t
    }

  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  val iter : (int -> 'a -> unit) -> 'a t -> unit
end

(** named export *)
type export =
  { name : string
  ; id : int
  }

(** named exports of a module *)
type exports =
  { global : export list
  ; mem : export list
  ; table : export list
  ; func : export list
  }

(** the type of simplified modules *)
type simplified_module =
  { id : string option
  ; global : (Types.Const.expr Types.global', Types.global_type) runtime Named.t
  ; table : (Types.table, Types.table_type) runtime Named.t
  ; mem : (Types.mem, Types.limits) runtime Named.t
  ; func : ((int, Types.func_type) Types.func', Types.func_type) runtime Named.t
  ; elem : (int, Types.Const.expr) Types.elem' Named.t
  ; data : (int, Types.Const.expr) Types.data' Named.t
  ; exports : exports
  ; start : int list
  }

(** pretty print a simplified module *)
val pp : Format.formatter -> simplified_module -> unit

(** simplify a module *)
val module_ : Types.module_ -> (simplified_module, string) Result.t

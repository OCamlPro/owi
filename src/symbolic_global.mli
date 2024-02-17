type t

type collection

val init : unit -> collection

val clone : collection -> collection

val get_global : Env_id.t -> Concrete_global.t -> collection -> int -> t

val typ : t -> Types.simplified Types.val_type

val mut : t -> Types.mut

val value : t -> Symbolic_value.S.t

val set_value : t -> Symbolic_value.S.t -> unit

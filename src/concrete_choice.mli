(* TODO: make this abstract *)
type 'a t = 'a

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val map : 'a t -> ('a -> 'b) -> 'b t

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

val select : bool -> bool t

val select_i32 : int32 -> int32 t

val get : unit

val trap : Trap.t -> 'a t

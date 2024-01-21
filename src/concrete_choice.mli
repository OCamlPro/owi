(* TODO: make this abstract *)
type 'a t = 'a

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b) -> 'b

val ( let* ) : 'a t -> ('a -> 'b) -> 'b

val select : bool -> bool t

val select_i32 : int32 -> int32 t

val get : unit

val trap : Trap.t -> 'a t

val ( let* ) : 'a Result.t -> ('a -> 'b Result.t) -> 'b Result.t

val ( let+ ) : 'a Result.t -> ('a -> 'b) -> 'b Result.t

val error : string -> 'a Result.t

val error_s : ('a, Format.formatter, unit, 'b Result.t) format4 -> 'a

val ok : 'a -> 'a Result.t

val list_iter : ('a -> unit Result.t) -> 'a list -> unit Result.t

val list_map : ('a -> 'b Result.t) -> 'a list -> 'b list Result.t

val list_fold_left : ('a -> 'b -> 'a Result.t) -> 'a -> 'b list -> 'a Result.t

val array_iter : ('a -> unit Result.t) -> 'a array -> unit Result.t

val array_map : ('a -> 'b Result.t) -> 'a array -> 'b array Result.t

val array_fold_left : ('a -> 'b -> 'a Result.t) -> 'a -> 'b array -> 'a Result.t

type (!'get, !'write) t

val init :
  (unit -> 'get option) -> ('write -> Condition.t -> unit) -> ('get, 'write) t

val get : ('get, 'write) t -> bool -> 'get option

val write : 'write -> ('get, 'write) t -> unit

val make_pledge : ('get, 'write) t -> unit

val end_pledge : ('get, 'write) t -> unit

val fail : ('get, 'write) t -> unit

val work_while : ('get -> ('write -> unit) -> unit) -> ('get, 'write) t -> unit

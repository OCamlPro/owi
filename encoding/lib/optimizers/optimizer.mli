open Z3_mappings

type t

exception Unknown

val solver_time : float ref
val create : unit -> t
val push : t -> unit
val pop : t -> unit
val add : t -> Expression.t list -> unit

val check :
  t ->
  Expression.t ->
  Expression.t list ->
  (t -> expr -> Z3.Optimize.handle) ->
  model Option.t

val maximize : t -> Expression.t -> Expression.t list -> Value.t option
val minimize : t -> Expression.t -> Expression.t list -> Value.t option

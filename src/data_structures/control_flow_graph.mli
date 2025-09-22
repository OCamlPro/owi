type t

val init :
  (int * Types.binary Types.expr) list -> (int * int * Int32.t option) list -> t

module Vertex : sig
  type t =
    { expr : Types.binary Types.expr
    ; idx : int
    }
end

module Edge : sig
  type t
end

(** Number of nodes in the graph. *)
val length : t -> int

val succ : t -> Vertex.t -> Vertex.t list

val pp : Format.formatter -> t -> unit

val iter_vertex : (Vertex.t -> unit) -> t -> unit

val fold_vertex : (Vertex.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val in_degree : t -> Vertex.t -> int

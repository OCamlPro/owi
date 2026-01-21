type t

val init : (int * Binary.expr) list -> (int * int * Int32.t option) list -> t

module Vertex : sig
  type t =
    { expr : Binary.expr
    ; idx : int
    }
end

module Edge : sig
  type t
end

(** Number of nodes in the graph. *)
val length : t -> int

val succ : t -> Vertex.t -> Vertex.t list

val pp : t Fmt.t

val iter_vertex : (Vertex.t -> unit) -> t -> unit

val fold_vertex : (Vertex.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val in_degree : t -> Vertex.t -> int

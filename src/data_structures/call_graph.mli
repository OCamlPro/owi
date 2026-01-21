module Vertex : sig
  type t =
    | Outside_world
    | Function of
        { idx : int
        ; cfg : Control_flow_graph.t option
        }
end

type t

val init :
  (int * Control_flow_graph.t option * Set.Make(Int).t) list -> int list -> t

val nb_vertex : t -> int

val pp : t Fmt.t

val iter_vertex : (Vertex.t -> unit) -> t -> unit

val iter_pred : (Vertex.t -> unit) -> t -> Vertex.t -> unit

val fold_vertex : (Vertex.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc

type 'a t

type _ g =
  | Cg : ('a option * bool) t g
  | Cfg : Types.binary Types.expr t g

val init_cg :
  (int * 'a option * Set.Make(Int).t) list -> int list -> ('a option * bool) t

val pp_cg : Format.formatter -> ('a option * bool) t -> unit

val init_cfg : (int * 'a) list -> (int * int * string option) list -> 'a t

val pp_cfg : Format.formatter -> 'a Types.expr t -> unit

val is_subgraph : 'a t -> 'a t -> bool

val compute_distance_to_unreachable_cg :
  (Types.binary Types.expr t option * bool) t -> int array array array

val pp_distances : Format.formatter -> int array array array -> unit

val kosaraju : 'a t -> Set.Make(Set.Make(Int)).t

val tarjan : 'a t -> Set.Make(Set.Make(Int)).t

val build_scc_graph : 'a t -> 'a t t

val pp_scc_graph : Format.formatter -> 'a t * 'a g -> unit

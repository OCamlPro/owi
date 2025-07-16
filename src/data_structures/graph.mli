type t

val init : (int * string option * int list) list -> int list -> t

val pp_dot : Format.formatter -> t -> unit

val is_subgraph : t -> t -> bool

val get_info : t -> string option

val kosaraju : t -> Set.Make(Set.Make(Int)).t

val tarjan : t -> Set.Make(Set.Make(Int)).t

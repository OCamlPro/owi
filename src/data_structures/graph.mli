type 'a t

type _ g =
  | Cg : (string option * bool) t g
  | Cfg : Types.binary Types.instr Annotated.t list t g

val init_cg : (int * string option * int list) list -> int list -> (string option * bool) t

val pp_cg : Format.formatter -> (string option * bool) t -> unit

val init_cfg : (int * 'a) list -> (int * int * string option) list -> 'a t

val pp_cfg : Format.formatter -> 'a Types.instr Annotated.t list t -> unit

val is_subgraph : 'a t -> 'a t -> bool

val kosaraju : 'a t -> Set.Make(Set.Make(Int)).t

val tarjan : 'a t -> Set.Make(Set.Make(Int)).t

val build_scc_graph : 'a t -> 'a t t

val pp_scc_graph : Format.formatter -> 'a t * 'a g -> unit

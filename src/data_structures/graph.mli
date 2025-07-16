type 'a t

val init_cg : (int * 'a * int list) list -> int list -> 'a t

val pp_cg : Format.formatter -> 'a t -> unit

val init_cfg : (int * 'a) list -> (int * int * string option) list -> 'a t

val pp_cfg : Format.formatter -> 'a Types.instr Annotated.t list t -> unit

val is_subgraph : 'a t -> 'a t -> bool

val kosaraju : 'a t -> Set.Make(Set.Make(Int)).t

val tarjan : 'a t -> Set.Make(Set.Make(Int)).t

val pp_scc_cg : Format.formatter -> 'a t * Set.Make(Set.Make(Int)).t -> unit

val pp_scc_cfg :
     Format.formatter
  -> 'a Types.instr Annotated.t list t * Set.Make(Set.Make(Int)).t
  -> unit

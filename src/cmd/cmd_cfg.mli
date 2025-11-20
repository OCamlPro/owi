val build_cfg_from_text_module : Text.Module.t -> int -> Control_flow_graph.t

val build_cfg_from_func : Binary.Func.t -> Control_flow_graph.t

val cmd : source_file:Fpath.t -> entry_point:string option -> unit Result.t

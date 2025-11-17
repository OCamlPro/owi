val build_cfg_from_text_module : Text.modul -> int -> Control_flow_graph.t

val build_cfg_from_func : Binary.func -> Control_flow_graph.t

val cmd : source_file:Fpath.t -> entry_point:string option -> unit Result.t

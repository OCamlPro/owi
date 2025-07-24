val build_cfg_from_text_module :
  Text.modul -> Types.binary Types.instr Annotated.t list Graph.t

val build_cfg_from_func :
  Types.binary Types.func -> Types.binary Types.instr Annotated.t list Graph.t

val cmd :
  source_file:Fpath.t -> entry_point:string option -> scc:bool -> unit Result.t

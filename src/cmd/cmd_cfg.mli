val build_cfg_from_text_module :
  Text.modul -> int -> Types.binary Types.expr Graph.t

val build_cfg_from_func :
  Types.binary Types.func -> Types.binary Types.expr Graph.t

val cmd :
  source_file:Fpath.t -> entry_point:string option -> scc:bool -> unit Result.t

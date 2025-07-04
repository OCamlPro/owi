type mode =
  | Call_graph
  | Control_flow_graph

val cmd :
     analyze_mode:mode
  -> call_graph_mode:Cmd_call_graph.mode
  -> source_file:Fpath.t
  -> entry_point:string option
  -> unit Result.t

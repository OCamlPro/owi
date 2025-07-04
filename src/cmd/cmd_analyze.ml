type mode =
  | Call_graph
  | Control_flow_graph

let cmd ~analyze_mode ~call_graph_mode ~source_file ~entry_point =
  match analyze_mode with
  | Call_graph -> Cmd_call_graph.cmd ~call_graph_mode ~source_file ~entry_point
  | _ -> Ok ()

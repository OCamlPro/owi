type node =
  { ind : int
  ; info : string option
  ; children : int list
  ; mutable visited : bool
  }

type t =
  { nodes : node list
  ; entry_points : int list
  }

let empty = { nodes = []; entry_points = [] }

let set_entry_points graph e = { nodes = graph.nodes; entry_points = e }

let add_edges graph i s l =
  let n = { ind = i; info = s; children = l; visited = false } in
  { nodes = n :: graph.nodes; entry_points = graph.entry_points }

let rec print_graph nodes acc k =
  match List.find_opt (fun n -> n.ind = k) nodes with
  | None -> acc
  | Some n ->
    if n.visited then acc
    else (
      n.visited <- true;
      List.fold_left
        (fun acc i ->
          print_graph nodes
            (String.concat ""
               [ acc; string_of_int k; "->"; string_of_int i; ";\n" ] )
            i )
        acc n.children )

let pp_dot fmt g =
  List.iter (fun n -> n.visited <- false) g.nodes;
  let s = List.fold_left (print_graph g.nodes) "" g.entry_points in
  Fmt.pf fmt "digraph call_graph {\n%s}" s

let find_indice g s =
  Option.map
    (fun n -> n.ind)
    (List.find_opt (fun n -> Option.equal String.equal n.info (Some s)) g.nodes)

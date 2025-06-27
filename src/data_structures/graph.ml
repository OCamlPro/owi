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

let set_entry_points graph e = { graph with entry_points = e }

let add_node graph i s l =
  let n = { ind = i; info = s; children = l; visited = false } in
  { graph with nodes = n :: graph.nodes }

let rec print_graph nodes acc k =
  match List.find_opt (fun n -> n.ind = k) nodes with
  | None -> acc
  | Some n ->
    if n.visited then acc
    else (
      n.visited <- true;
      List.fold_left
        (fun acc i -> print_graph nodes ((k, i) :: acc) i)
        acc n.children )

let pp_entry_points fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";") Fmt.int fmt l

let pp_edge fmt (n1, n2) = Fmt.pf fmt "%a -> %a" Fmt.int n1 Fmt.int n2

let pp_edges fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";\n") pp_edge fmt l

let pp_dot fmt g =
  List.iter (fun n -> n.visited <- false) g.nodes;
  let l = List.fold_left (print_graph g.nodes) [] g.entry_points in
  Fmt.pf fmt "digraph call_graph {\n%a;\n%a}" pp_entry_points g.entry_points
    pp_edges l

let find_indice g s =
  Option.map
    (fun n -> n.ind)
    (List.find_opt (fun n -> Option.equal String.equal n.info (Some s)) g.nodes)

module S = Set.Make (Int)

type node =
  { ind : int
  ; info : string option
  ; children : int list
  }

type t =
  { nodes : node array
  ; entry_points : int list
  }

let init l entry_points =
  let l' = List.sort (fun (n1, _, _) (n2, _, _) -> compare n1 n2) l in
  let nodes =
    Array.of_list
      (List.map (fun (ind, info, children) -> { ind; info; children }) l')
  in
  { nodes; entry_points }

let rec print_graph nodes (acc, visited) (n : node) =
  if S.mem n.ind visited then (acc, visited)
  else
    let visited = S.add n.ind visited in
    List.fold_left
      (fun (acc, visited) (x : int) ->
        if x < Array.length nodes then
          let i = Array.get nodes x in
          print_graph nodes ((n.ind, i.ind) :: acc, visited) i
        else (acc, visited) )
      (acc, visited) n.children

let pp_entry_points fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";") Fmt.int fmt l

let pp_edge fmt (n1, n2) = Fmt.pf fmt "%a -> %a" Fmt.int n1 Fmt.int n2

let pp_edges fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";\n") pp_edge fmt l

let pp_dot fmt g =
  let entry_points =
    List.concat_map
      (fun x ->
        Option.to_list
          (if x < Array.length g.nodes then Some (Array.get g.nodes x) else None) )
      g.entry_points
  in
  let l, _ = List.fold_left (print_graph g.nodes) ([], S.empty) entry_points in
  Fmt.pf fmt "digraph call_graph {\n%a;\n%a}" pp_entry_points g.entry_points
    pp_edges l

let get_info graph = (Array.get graph.nodes 1).info (* sert à utilser info *)

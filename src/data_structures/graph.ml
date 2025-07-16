module S = Set.Make (Int)
module Partition = Set.Make (Set.Make (Int))
module M = Map.Make (Int)

type node =
  { ind : int
  ; info : string option
  ; children : int list
  ; parents : int list
  }

type t =
  { nodes : node array
  ; entry_points : int list
  }

let init l entry_points =
  let l' = List.sort (fun (n1, _, _) (n2, _, _) -> compare n1 n2) l in

  let parents_map =
    List.fold_left
      (fun map (ind, _, children) ->
        List.fold_left (fun map c -> M.add_to_list c ind map) map children )
      M.empty l
  in
  let nodes =
    Array.of_list
      (List.map
         (fun (ind, info, children) ->
           { ind
           ; info
           ; children
           ; parents = Option.value (M.find_opt ind parents_map) ~default:[]
           } )
         l' )
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

let rec compare_children l1 l2 =
  match (l1, l2) with
  | _, [] -> true
  | [], _ -> false
  | h1 :: t1, h2 :: t2 ->
    if h1 = h2 then compare_children t1 t2
    else if h1 < h2 then compare_children t1 l2
    else false

let rec compare_nodes l1 l2 =
  match (l1, l2) with
  | _, [] -> true
  | [], _ -> false
  | h1 :: t1, h2 :: t2 ->
    if h1.ind = h2.ind && compare_children h1.children h2.children then
      compare_nodes t1 t2
    else if h1.ind < h2.ind then compare_nodes t1 l2
    else false

let is_subgraph graph subgraph =
  let res =
    List.fold_left2
      (fun acc n1 n2 -> n1 = n2 && acc)
      true graph.entry_points subgraph.entry_points
  in
  compare_nodes (Array.to_list graph.nodes) (Array.to_list subgraph.nodes)
  && res

let get_info graph = (Array.get graph.nodes 1).info (* just to use info *)

let rec first_explore graph acc node =
  let visited, post_order = acc in
  let visited = S.add node visited in
  let parents = (Array.get graph.nodes node).parents in
  let visited, post_order =
    List.fold_left
      (fun acc u ->
        let visited = fst acc in
        if not (S.mem u visited) then first_explore graph acc u else acc )
      (visited, post_order) parents
  in
  (visited, node :: post_order)

let rec snd_explore graph acc node =
  let visited, component = acc in
  let visited = S.add node visited in
  let children = (Array.get graph.nodes node).children in
  let visited, component =
    List.fold_left
      (fun acc u ->
        let visited = fst acc in
        if not (S.mem u visited) then snd_explore graph acc u else acc )
      (visited, component) children
  in
  (visited, S.add node component)

let kosaraju graph =
  let _, post_order =
    Array.fold_left
      (fun acc u ->
        let visited = fst acc in
        if not (S.mem u.ind visited) then first_explore graph acc u.ind else acc )
      (S.empty, []) graph.nodes
  in
  let _, partition =
    List.fold_left
      (fun acc u ->
        let visited, partition = acc in
        if not (S.mem u visited) then
          let visited, component = snd_explore graph (visited, S.empty) u in
          (visited, Partition.add component partition)
        else acc )
      (S.empty, Partition.empty) post_order
  in
  partition

type infos =
  { num : int
  ; num_accessible : int
  ; in_stack : bool
  }

let rec explore graph acc node =
  let children = (Array.get graph.nodes node).children in
  let num, stack, partition, visited = acc in
  let visited =
    M.add node { num; num_accessible = num; in_stack = true } visited
  in
  let stack = node :: stack in
  let num = num + 1 in

  let num, stack, partition, visited =
    List.fold_left
      (fun acc w ->
        let num, stack, partition, visited = acc in
        match M.find_opt w visited with
        | None ->
          let num, stack, partition, visited = explore graph acc w in
          let w =
            match M.find_opt w visited with Some w -> w | None -> assert false
          in
          let visited =
            M.update node
              (fun v_opt ->
                Option.bind v_opt (fun v ->
                  Some
                    { v with
                      num_accessible = min v.num_accessible w.num_accessible
                    } ) )
              visited
          in
          (num, stack, partition, visited)
        | Some w ->
          if w.in_stack then
            let visited =
              M.update node
                (fun v_opt ->
                  Option.bind v_opt (fun v ->
                    Some { v with num_accessible = min v.num_accessible w.num } ) )
                visited
            in
            (num, stack, partition, visited)
          else (num, stack, partition, visited) )
      (num, stack, partition, visited)
      children
  in

  let v =
    match M.find_opt node visited with Some v -> v | None -> assert false
  in
  if v.num_accessible = v.num then
    let rec build_component stack visited component =
      let w, stack = match stack with [] -> assert false | h :: t -> (h, t) in
      let visited =
        M.update w
          (fun w_opt ->
            Option.bind w_opt (fun w -> Some { w with in_stack = false }) )
          visited
      in
      let component = S.add w component in
      if node = w then (stack, visited, component)
      else build_component stack visited component
    in
    let stack, visited, component = build_component stack visited S.empty in
    (num, stack, Partition.add component partition, visited)
  else (num, stack, partition, visited)

let tarjan graph =
  let _, _, partition, _ =
    Array.fold_left
      (fun acc u ->
        let _, _, _, visited = acc in
        if not (M.mem u.ind visited) then explore graph acc u.ind else acc )
      (0, [], Partition.empty, M.empty)
      graph.nodes
  in
  partition

let pp_subgraph (subgraphs, l, nodes) fmt n =
  let nodes = List.filter (fun e -> Array.get subgraphs e.ind = n) nodes in
  let l =
    List.filter
      (fun (n1, n2) -> Array.get subgraphs n1 = n && Array.get subgraphs n2 = n)
      l
  in
  let nodes = List.map (fun n -> n.ind) nodes in
  Fmt.pf fmt "subgraph cluster_%a {\n%a;\n%a}" Fmt.int n pp_entry_points nodes
    pp_edges l

let pp_subgraphs fmt (subgraphs, l, entry_points, len) =
  Fmt.list
    ~sep:(fun fmt () -> Fmt.pf fmt ";\n")
    (pp_subgraph (subgraphs, l, entry_points))
    fmt len

let pp_edge_scc fmt (n1, n2, c1, c2) =
  Fmt.pf fmt "%a -> %a [ltail=cluster_%a,lhead=cluster_%a]" Fmt.int n1 Fmt.int
    n2 Fmt.int c1 Fmt.int c2

let pp_edges_scc fmt (subgraphs, l) =
  let l =
    List.filter
      (fun (n1, n2) -> not (Array.get subgraphs n1 = Array.get subgraphs n2))
      l
  in
  let l =
    List.map
      (fun (n1, n2) ->
        let c1 = Array.get subgraphs n1
        and c2 = Array.get subgraphs n2 in
        (n1, n2, c1, c2) )
      l
  in
  let l =
    List.sort_uniq
      (fun (_, _, x1, y1) (_, _, x2, y2) ->
        compare ((x1 * 10) + y1) ((x2 * 10) + y2) )
      l
  in
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";\n") pp_edge_scc fmt l

let pp_scc fmt (g, partition) =
  let partition = Partition.to_list partition in

  let nb_clusters = List.length partition in
  let subgraphs =
    Array.init (Array.length g.nodes) (fun i ->
      match List.find_index (fun s -> S.mem i s) partition with
      | Some g -> g
      | None -> assert false )
  in

  let entry_points =
    List.concat_map
      (fun x ->
        Option.to_list
          (if x < Array.length g.nodes then Some (Array.get g.nodes x) else None) )
      g.entry_points
  in
  let l, _ = List.fold_left (print_graph g.nodes) ([], S.empty) entry_points in
  Fmt.pf fmt "digraph g {\ngraph [compound=true];\n%a;\n%a}" pp_subgraphs
    (subgraphs, l, Array.to_list g.nodes, List.init nb_clusters (fun i -> i))
    pp_edges_scc (subgraphs, l)

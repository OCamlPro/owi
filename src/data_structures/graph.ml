module S = Set.Make (Int)
module Partition = Set.Make (Set.Make (Int))
module M = Map.Make (Int)

type 'a node =
  { ind : int
  ; info : 'a
  ; children : (int * string option) list
  ; parents : int list
  }

type 'a t =
  { nodes : 'a node array
  ; entry_points : int list
  }

type _ g =
  | Cg : string option t g
  | Cfg : Types.binary Types.instr Annotated.t list t g

let init_cg l entry_points =
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
           let children = List.map (fun x -> (x, None)) children in
           { ind
           ; info
           ; children
           ; parents = Option.value (M.find_opt ind parents_map) ~default:[]
           } )
         l' )
  in
  { nodes; entry_points }

let rec print_graph nodes (acc, visited) n =
  if S.mem n.ind visited then (acc, visited)
  else
    let visited = S.add n.ind visited in
    List.fold_left
      (fun (acc, visited) (x, _) ->
        if x < Array.length nodes then
          let i = Array.get nodes x in
          print_graph nodes ((n.ind, i.ind) :: acc, visited) i
        else (acc, visited) )
      (acc, visited) n.children

let pp_entry_points fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";") Fmt.int fmt l

let pp_edge_cg fmt (n1, n2) = Fmt.pf fmt "%a -> %a" Fmt.int n1 Fmt.int n2

let pp_edges_cg fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";\n") pp_edge_cg fmt l

let pp_cg fmt (g : string option t) =
  let entry_points =
    List.concat_map
      (fun x ->
        Option.to_list
          (if x < Array.length g.nodes then Some (Array.get g.nodes x) else None) )
      g.entry_points
  in
  let l, _ = List.fold_left (print_graph g.nodes) ([], S.empty) entry_points in
  Fmt.pf fmt "digraph call_graph {\n%a;\n%a}" pp_entry_points g.entry_points
    pp_edges_cg l

let pp_subgraph_cg (subgraphs, l, nodes) fmt n =
  let nodes = List.filter (fun e -> Array.get subgraphs e.ind = n) nodes in
  let l =
    List.filter
      (fun (n1, n2) -> Array.get subgraphs n1 = n && Array.get subgraphs n2 = n)
      l
  in
  let nodes = List.map (fun n -> n.ind) nodes in
  Fmt.pf fmt "subgraph cluster_%a {\n %a;\n %a}" Fmt.int n pp_entry_points nodes
    pp_edges_cg l

let pp_subgraphs_cg fmt (subgraphs, l, nodes, len) =
  Fmt.list
    ~sep:(fun fmt () -> Fmt.pf fmt ";\n ")
    (pp_subgraph_cg (subgraphs, l, nodes))
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
        match compare x1 x2 with 0 -> compare y1 y2 | x -> x )
      l
  in
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";\n ") pp_edge_scc fmt l

let pp_scc_cg fmt (g, partition) =
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
  Fmt.pf fmt "digraph g {\n graph [compound=true];\n %a;\n %a}" pp_subgraphs_cg
    (subgraphs, l, Array.to_list g.nodes, List.init nb_clusters (fun i -> i))
    pp_edges_scc (subgraphs, l)

let init_cfg nodes edges =
  let children_map, parents_map =
    List.fold_left
      (fun (m_children, m_parents) (n1, n2, s) ->
        (M.add_to_list n1 (n2, s) m_children, M.add_to_list n2 n1 m_parents) )
      (M.empty, M.empty) edges
  in
  let l =
    List.rev
      (List.map
         (fun (ind, info) ->
           { ind
           ; info
           ; children = Option.value (M.find_opt ind children_map) ~default:[]
           ; parents = Option.value (M.find_opt ind parents_map) ~default:[]
           } )
         nodes )
  in
  { nodes = Array.of_list l; entry_points = [] }

let pp_inst fmt i = Fmt.pf fmt "%a" (Types.pp_instr ~short:true) i.Annotated.raw

let pp_exp fmt l = Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " | ") pp_inst fmt l

let pp_label fmt s = Fmt.pf fmt {|[label="%a"]|} Fmt.string s

let pp_label_opt fmt s_opt = Fmt.option pp_label fmt s_opt

let pp_edge_cfg n1 fmt (n2, s) =
  Fmt.pf fmt "%a -> %a %a;" Fmt.int n1 Fmt.int n2 pp_label_opt s

let pp_edges_cfg fmt (n, l) =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt "\n") (pp_edge_cfg n) fmt l

let pp_node fmt (n : 'a node) =
  Fmt.pf fmt {|%a [label="%a"]; %a|} Fmt.int n.ind pp_exp (List.rev n.info)
    pp_edges_cfg (n.ind, n.children)

let pp_nodes fmt g =
  Fmt.array ~sep:(fun fmt () -> Fmt.pf fmt "\n") pp_node fmt g

let pp_cfg_graph fmt g = Fmt.pf fmt "%a" pp_nodes g.nodes

let pp_cfg fmt g =
  Fmt.pf fmt "digraph cfg {\n rankdir=LR;\n node [shape=record] ; %a}"
    pp_cfg_graph g

let rec compare_children l1 l2 =
  match (l1, l2) with
  | _, [] -> true
  | [], _ -> false
  | (h1, _) :: t1, (h2, _) :: t2 ->
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
      (fun acc (u, _) ->
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
      (fun acc (w, _) ->
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

let build_subgraph subgraphs subgraph_head graph n =
  let nodes = Array.to_list graph.nodes in
  let nodes = List.filter (fun e -> Array.get subgraphs e.ind = n) nodes in
  let nodes, children, parents =
    List.fold_left
      (fun (nodes, children, parents) node ->
        let children_in, children_out =
          List.fold_left
            (fun (in_c, out_c) (x, s) ->
              let sg = Array.get subgraphs x in
              if sg = n then ((x, s) :: in_c, out_c)
              else (in_c, (Array.get subgraph_head sg, None) :: out_c) )
            ([], []) node.children
        in

        let parents_in, parents_out =
          List.fold_left
            (fun (in_p, out_p) x ->
              let sg = Array.get subgraphs x in
              if sg = n then (x :: in_p, out_p)
              else (in_p, Array.get subgraph_head sg :: out_p) )
            ([], []) node.parents
        in

        ( { node with children = children_in; parents = parents_in } :: nodes
        , children_out :: children
        , parents_out :: parents ) )
      ([], [], []) nodes
  in

  let entry_points =
    List.filter (fun x -> Array.get subgraphs x = n) graph.entry_points
  in
  let children =
    List.sort_uniq (fun x y -> compare (fst x) (fst y)) (List.concat children)
  in
  let parents = List.sort_uniq (fun x y -> compare x y) (List.concat parents) in

  let ind = Array.get subgraph_head n in
  let sgraph = { nodes = Array.of_list (List.rev nodes); entry_points } in
  (* List.rev ? *)
  { ind; info = sgraph; children; parents }

let build_scc_graph graph =
  let partition = Partition.to_list (tarjan graph) in
  let nb_subgraphs = List.length partition in

  let subgraph_head =
    let partition = Array.of_list partition in
    Array.init nb_subgraphs (fun i ->
      let e = Array.get partition i in
      match S.choose_opt e with Some i -> i | None -> assert false )
  in

  let subgraphs =
    Array.init (Array.length graph.nodes) (fun i ->
      match List.find_index (fun s -> S.mem i s) partition with
      | Some g -> g
      | None -> assert false )
  in

  let nodes =
    List.map
      (fun n -> build_subgraph subgraphs subgraph_head graph n)
      (List.init nb_subgraphs (fun i -> i))
  in
  let entry_points =
    List.map (fun n -> Array.get subgraphs n) graph.entry_points
  in
  let entry_points = List.sort_uniq compare entry_points in
  { nodes = Array.of_list nodes; entry_points }

let pp_scc_edge n1 fmt (n2, _) =
  Fmt.pf fmt "%a -> %a [ltail=cluster_%a,lhead=cluster_%a]" Fmt.int n1 Fmt.int
    n2 Fmt.int n1 Fmt.int n2

let pp_scc_edges fmt (n, l) =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt "\n") (pp_scc_edge n) fmt l

let pp_scc_node pp fmt n =
  Fmt.pf fmt "subgraph cluster_%a {\n %a };\n %a" Fmt.int n.ind pp n.info
    pp_scc_edges (n.ind, n.children)

let pp_scc_nodes fmt (n, pp) =
  Fmt.array ~sep:(fun fmt () -> Fmt.pf fmt "\n") (pp_scc_node pp) fmt n

let pp_scc_graph (type a) fmt ((graph : a t), (g_type : a g)) =
  let (pp : Format.formatter -> a -> unit) =
    match g_type with Cg -> pp_cg | Cfg -> pp_cfg_graph
  in
  Fmt.pf fmt
    "digraph scc_graph {\n\
    \ graph [compound=true];\n\
    \ rankdir=LR;\n\
    \ node [shape=record] ; %a}"
    pp_scc_nodes (graph.nodes, pp)

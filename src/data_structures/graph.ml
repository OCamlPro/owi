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
  | Cg : ('a option * bool) t g
  | Cfg : Types.binary Types.expr t g

let init_cg (l : (int * 'a option * Set.Make(Int).t) list) entry_points =
  let l' = List.sort (fun (n1, _, _) (n2, _, _) -> compare n1 n2) l in

  let parents_map =
    List.fold_left
      (fun map (ind, _, children) ->
        S.fold (fun c map -> M.add_to_list c ind map) children map )
      M.empty l
  in
  let nodes =
    Array.of_list
      (List.map
         (fun (ind, info, children) ->
           let children = S.to_list children in
           let children = List.map (fun x -> (x, None)) children in
           let parents =
             Option.value (M.find_opt ind parents_map) ~default:[]
           in

           let b =
             match parents with
             | [] -> List.exists (fun n -> ind = n) entry_points
             | _ -> true
           in

           let children, parents, info =
             if b then (children, parents, info) else ([], [], None)
           in
           { ind; info = (info, b); children; parents } )
         l' )
  in
  { nodes; entry_points }

let pp_sep fmt () = Fmt.pf fmt "@,"

let pp_label fmt s = Fmt.pf fmt {|[label="%a"]|} Fmt.string s

let pp_label_opt fmt s_opt = Fmt.option pp_label fmt s_opt

let pp_edge n1 fmt (n2, s) = Fmt.pf fmt "%d -> %d%a" n1 n2 pp_label_opt s

let pp_edges fmt (n, l) = Fmt.list ~sep:pp_sep (pp_edge n) fmt l

let pp_node_cg fmt (n : 'a node) =
  if snd n.info then Fmt.pf fmt "%d%a%a" n.ind pp_sep () pp_edges (n.ind, n.children) 

let pp_nodes_cg fmt n = 
  let nodes = List.filter (fun n -> snd n.info) (Array.to_list n) in
  Fmt.list ~sep:pp_sep pp_node_cg fmt nodes

let pp_cg_graph fmt g = Fmt.pf fmt "%a" pp_nodes_cg g.nodes

let pp_cg fmt g = Fmt.pf fmt "@[<v 2>digraph call_graph {@,%a}@]" pp_cg_graph g

let init_cfg nodes edges =
  let children_map, parents_map =
    List.fold_left
      (fun (m_children, m_parents) (n1, n2, s) ->
        (M.add_to_list n1 (n2, s) m_children, M.add_to_list n2 n1 m_parents) )
      (M.empty, M.empty) edges
  in
  let l =
    List.rev_map
      (fun (ind, info) ->
        { ind
        ; info
        ; children = Option.value (M.find_opt ind children_map) ~default:[]
        ; parents = Option.value (M.find_opt ind parents_map) ~default:[]
        } )
      nodes
  in
  { nodes = Array.of_list l; entry_points = [] }

let pp_inst fmt i = Fmt.pf fmt "%a" (Types.pp_instr ~short:true) i.Annotated.raw

let pp_exp fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.string fmt " | ") pp_inst fmt l

let pp_node_cfg fmt (n : 'a node) =
  Fmt.pf fmt {|%d [label="%a"]%a%a|} n.ind pp_exp (List.rev n.info) pp_sep () pp_edges
    (n.ind, n.children)

let pp_nodes_cfg fmt g = Fmt.array ~sep:pp_sep pp_node_cfg fmt g

let pp_cfg_graph fmt g = Fmt.pf fmt "%a" pp_nodes_cfg g.nodes

let pp_cfg fmt g =
  Fmt.pf fmt "@[<v 2>digraph cfg {@,rankdir=LR;@,node [shape=record];@,%a}@]"
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

module IntPair = struct
  (* this should be replaced by Pair module in 5.4 *)
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    let c = compare x1 x2 in
    if c <> 0 then c else compare y1 y2
end

module Calls = Map.Make (IntPair)

let find_unreachables_cfg cg acc calls
  (graph : Types.binary Types.instr Annotated.t list t) =
  let res, calls, _ =
    Array.fold_left
      (fun (acc, calls, i) node ->
        match node.info with
        (* | { Annotated.raw = Types.Unreachable; _ } :: _ -> (
          match node.parents with
          | [] ->
            if i = 0 then ((cg, i) :: acc, calls, i + 1) else (acc, calls, i + 1)
          | _ -> ((cg, i) :: acc, calls, i + 1) ) *)
        | { Annotated.raw = Types.Call (Raw 1); _ } :: _ -> (
          (* only to test with the benchs *)
          match node.parents with
          | [] ->
            if i = 0 then ((cg, i) :: acc, calls, i + 1) else (acc, calls, i + 1)
          | _ -> ((cg, i) :: acc, calls, i + 1) )
        | { Annotated.raw =
              Types.(
                ( Call _ | Call_indirect _ | Return_call _
                | Return_call_indirect _ ))
          ; Annotated.functions_called = funcs
          ; _
          }
          :: _ ->
          let calls =
            S.fold (fun f acc -> Calls.add_to_list (cg, f) i acc) funcs calls
          in
          (acc, calls, i + 1)
        | _ -> (acc, calls, i + 1) )
      (acc, calls, 0) graph.nodes
  in
  (res, calls)

let find_unreachables_cg graph =
  let res, calls, _ =
    Array.fold_left
      (fun (acc, calls, i) node ->
        match node.info with
        | Some g, _ ->
          let acc, calls = find_unreachables_cfg i acc calls g in
          (acc, calls, i + 1)
        | _ -> (acc, calls, i + 1) )
      ([], Calls.empty, 0) graph.nodes
  in
  (res, calls)

let get_children_true_false node =
  match node.children with
  | [ (t, Some "true"); (f, Some "false") ]
  | [ (f, Some "false"); (t, Some "true") ] ->
    (t, f)
  | _ -> assert false

let set_instr_distances node distances cg =
  match node.info with
  | [] -> ()
  | h :: _ -> (
    match h.Annotated.raw with
    | Types.(If_else _ | Br_if _) ->
      let t, f = get_children_true_false node in
      Annotated.set_d_true h distances.(cg).(t);
      Annotated.set_d_false h distances.(cg).(f)
    | _ -> () )

let rec distance_unreachable_cfg nodes d distances unreachable cg cfg =
  let node = nodes.(cfg) in
  let d = match node.children with [] | [ _ ] -> d | _ -> d + 1 in
  let d' = distances.(cg).(cfg).(unreachable) in
  if d < d' then (
    distances.(cg).(cfg).(unreachable) <- d;
    set_instr_distances node distances cg;
    List.iter
      (distance_unreachable_cfg nodes d distances unreachable cg)
      node.parents )

let rec distance_unreachable_cg nodes calls d distances unreachable (cg, cfg) =
  let node = nodes.(cg) in

  match node.info with
  | Some g, _ ->
    distance_unreachable_cfg g.nodes d distances unreachable cg cfg;
    List.iter
      (fun p ->
        match Calls.find_opt (p, cg) calls with
        | Some l ->
          List.iter
            (fun node ->
              let d = distances.(cg).(0).(unreachable) in
              let d' = distances.(p).(node).(unreachable) in
              if d < d' then
                distance_unreachable_cg nodes calls d distances unreachable
                  (p, node) )
            l
        | None -> () )
      node.parents
  | None, _ -> assert false

let compute_distance_to_unreachable_cg graph =
  let unreachables, calls = find_unreachables_cg graph in
  let nb_unr = List.length unreachables in
  let distances =
    Array.init (Array.length graph.nodes) (fun i ->
      let length =
        match graph.nodes.(i).info with
        | Some g, _ -> Array.length g.nodes
        | None, _ -> 0
      in
      Array.make_matrix length nb_unr Int.max_int )
  in
  List.iteri
    (distance_unreachable_cg graph.nodes calls 0 distances)
    unreachables;
  distances

let pp_unreachables unreachables =
  Fmt.array ~sep:(fun fmt () -> Fmt.string fmt " - ") Fmt.int unreachables

let pp_distances2 fmt d =
  if Array.length d > 0 then Fmt.pf fmt "-> %a" pp_unreachables d.(0)

let pp_distances fmt distances =
  Fmt.array ~sep:(fun fmt () -> Fmt.string fmt "\n") pp_distances2 fmt distances

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
              (function
                | None -> None
                | Some v ->
                  Some
                    { v with
                      num_accessible = min v.num_accessible w.num_accessible
                    } )
              visited
          in
          (num, stack, partition, visited)
        | Some w ->
          if w.in_stack then
            let visited =
              M.update node
                (function
                  | None -> None
                  | Some v ->
                    Some { v with num_accessible = min v.num_accessible w.num } )
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
          (function None -> None | Some w -> Some { w with in_stack = false })
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
  Fmt.pf fmt "%d -> %d [ltail=cluster_%d,lhead=cluster_%d]" n1 n2 n1 n2

let pp_scc_edges fmt (n, l) =
  Fmt.list ~sep:pp_sep (pp_scc_edge n) fmt l

let pp_scc_node pp fmt n =
  Fmt.pf fmt "subgraph cluster_%d {@,%a};@,%a" n.ind pp n.info pp_scc_edges
    (n.ind, n.children)

let pp_scc_nodes fmt (n, pp) =
  Fmt.array ~sep:pp_sep (pp_scc_node pp) fmt n

let pp_scc_graph (type a) fmt ((graph : a t), (g_type : a g)) =
  match g_type with
  | Cg ->
    Fmt.pf fmt "@[<v 2>digraph scc_graph {@,graph [compound=true];@,%a}@]" pp_scc_nodes
      (graph.nodes, pp_cg_graph)
  | Cfg ->
    Fmt.pf fmt
      "@[<v 2>digraph scc_graph {@,graph [compound=true];@,rankdir=LR;@,node [shape=record];@,%a}@]"
      pp_scc_nodes
      (graph.nodes, pp_cfg_graph)

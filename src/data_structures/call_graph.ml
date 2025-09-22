module Vertex = struct
  (** A node of the call graph is either a Wasm function, either the "outside
      world". *)
  type t =
    | Outside_world
    | Function of
        { idx : int  (** The index of the function in the binary module. *)
        ; cfg : Control_flow_graph.t option
            (** The CFG representing the body of the function. When this is an
                imported function and its body is not available it will be
                `None`. *)
        }

  let compare n1 n2 =
    match (n1, n2) with
    | Function n1, Function n2 -> Int.compare n1.idx n2.idx
    | Outside_world, Outside_world -> 0
    | Outside_world, Function _n -> -1
    | Function _, Outside_world -> 1

  let hash = Hashtbl.hash

  let equal n1 n2 =
    match (n1, n2) with
    | Function n1, Function n2 -> Int.equal n1.idx n2.idx
    | Outside_world, Outside_world -> true
    | _ -> false
end

module G = Graph.Persistent.Digraph.Concrete (Vertex)
include G
module IntSet = Set.Make (Int)

let init (l : (int * Control_flow_graph.t option * Set.Make(Int).t) list)
  entry_points =
  let graph = empty in

  let graph = add_vertex graph Vertex.Outside_world in

  let tbl = Hashtbl.create 512 in

  (* adding all vertices *)
  let graph =
    List.fold_left
      (fun graph (idx, cfg, _children) ->
        let f = Vertex.Function { idx; cfg } in
        Hashtbl.add tbl idx f;
        add_vertex graph f )
      graph l
  in

  (* add all edges from functions to functions *)
  let graph =
    List.fold_left
      (fun graph (idx, _cfg, children) ->
        let parent =
          match Hashtbl.find_opt tbl idx with
          | None -> assert false
          | Some parent -> parent
        in
        IntSet.fold
          (fun child_idx graph ->
            let child =
              match Hashtbl.find_opt tbl child_idx with
              | None -> assert false
              | Some child -> child
            in
            add_edge graph parent child )
          children graph )
      graph l
  in

  (* add all edges from the outside world to entry points functions *)
  let graph =
    List.fold_left
      (fun graph entry_point_idx ->
        let entry_point =
          match Hashtbl.find_opt tbl entry_point_idx with
          | None -> assert false
          | Some entry_point -> entry_point
        in
        add_edge graph Vertex.Outside_world entry_point )
      graph entry_points
  in

  graph

let pp_sep fmt () = Fmt.pf fmt "@,"

let id_of_node = function
  | Vertex.Outside_world -> ~-1
  | Function { idx; _ } -> idx

let pp_edge fmt (n1, n2) =
  let n1 = id_of_node n1 in
  let n2 = id_of_node n2 in
  Fmt.pf fmt "%d -> %d" n1 n2

let pp_edges g fmt vertex =
  iter_succ
    (fun succ ->
      pp_edge fmt (vertex, succ);
      pp_sep fmt () )
    g vertex;
  pp_sep fmt ()

let pp_name fmt node = Fmt.int fmt (id_of_node node)

let pp_vertex g fmt vertex =
  Fmt.pf fmt "%a@,%a" pp_name vertex (pp_edges g) vertex

let pp_vertices fmt g =
  (* TODO: add an option to remove nodes with no ancestors and that are not entry-points? *)
  Fmt.iter ~sep:pp_sep iter_vertex (pp_vertex g) fmt g

let pp fmt (g : t) =
  Fmt.pf fmt "@[<v 2>digraph call_graph {@,%a}@]" pp_vertices g

module Condensate = struct
  module FunctionSet = struct
    include Set.Make (Vertex)

    let hash s = Hashtbl.hash s
  end

  module G = Graph.Persistent.Digraph.Concrete (FunctionSet)
  include G
end

module type Empty = sig end

let () =
  let ignore _ = () in
  ignore (module Condensate : Empty);
  ()

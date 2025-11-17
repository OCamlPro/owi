module Vertex = struct
  (** A node of the control flow graph is an expression. The invariant is that
      it should not contain any "block". *)
  type t =
    { expr : Binary.expr
    ; idx : int
    }

  let compare n1 n2 = Int.compare n1.idx n2.idx

  let hash n = Int.hash n.idx

  let equal n1 n2 = Int.equal n1.idx n2.idx
end

module Edge = struct
  (** An edge is a value on which we will branch. The option is used for the
      "default" case. *)
  type t = Int32.t option

  let compare n1 n2 = Option.compare Int32.compare n1 n2

  let default = None
end

module G = Graph.Persistent.Digraph.ConcreteLabeled (Vertex) (Edge)
include G

let init nodes edges =
  let graph = empty in

  let tbl = Hashtbl.create 512 in

  (* adding all vertices *)
  let graph =
    List.fold_left
      (fun graph (idx, expr) ->
        let vertex = { Vertex.expr; idx } in
        Hashtbl.add tbl idx vertex;
        add_vertex graph vertex )
      graph nodes
  in

  (* adding all edges *)
  List.fold_left
    (fun graph (parent, child, branch) ->
      let parent =
        match Hashtbl.find_opt tbl parent with
        | None -> assert false
        | Some parent -> parent
      in
      let child =
        match Hashtbl.find_opt tbl child with
        | None -> assert false
        | Some child -> child
      in
      let edge = E.create parent branch child in
      add_edge_e graph edge )
    graph edges

let length g = G.nb_vertex g

let pp_sep fmt () = Fmt.pf fmt "@,"

let pp_label fmt = function
  | None -> Fmt.pf fmt {|[label="default"]|}
  | Some v -> Fmt.pf fmt {|[label="%ld"]|} v

let pp_edge fmt edge =
  let src = E.src edge in
  let dst = E.dst edge in
  let lbl = E.label edge in
  Fmt.pf fmt "%d -> %d%a" src.Vertex.idx dst.Vertex.idx pp_label lbl

let pp_edges g fmt vertex =
  iter_succ_e
    (fun edge ->
      pp_edge fmt edge;
      pp_sep fmt () )
    g vertex;
  pp_sep fmt ()

let pp_inst fmt i =
  Fmt.pf fmt "%a" (Binary.pp_instr ~short:true) i.Annotated.raw

let pp_exp fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.string fmt " | ") pp_inst fmt l

let pp_vertex g fmt (vertex : Vertex.t) =
  let idx = vertex.idx in
  (* TODO: why do we have to use `List.rev` here ?! *)
  let expr = List.rev vertex.expr in
  Fmt.pf fmt {|%d [label="%a"]%a%a|} idx pp_exp expr pp_sep () (pp_edges g)
    vertex

let pp_vertices fmt g = Fmt.iter ~sep:pp_sep iter_vertex (pp_vertex g) fmt g

let pp fmt (g : t) =
  Fmt.pf fmt "@[<v 2>digraph cfg {@,rankdir=LR;@,node [shape=record];@,%a}@]"
    pp_vertices g

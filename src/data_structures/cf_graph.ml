module M = Map.Make (Int)

type 'a node =
  { ind : int
  ; info : 'a
  ; children : (int * string option) list
  }

type 'a t = 'a node array

let init nodes edges =
  let m =
    List.fold_left
      (fun map (n1, n2, s) -> M.add_to_list n1 (n2, s) map)
      M.empty edges
  in
  let l =
    List.rev
      (List.map
         (fun (ind, info) ->
           { ind; info; children = Option.value (M.find_opt ind m) ~default:[] } )
         nodes )
  in
  Array.of_list l

let pp_inst fmt i = Fmt.pf fmt "%a" (Types.pp_instr ~short:true) i.Annotated.raw

let pp_exp fmt l = Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " | ") pp_inst fmt l

let pp_label fmt s = Fmt.pf fmt {|[label="%a"]|} Fmt.string s

let pp_label_opt fmt s_opt = Fmt.option pp_label fmt s_opt

let pp_edge n1 fmt (n2, s) =
  Fmt.pf fmt "%a -> %a %a;" Fmt.int n1 Fmt.int n2 pp_label_opt s

let pp_edges fmt (n, l) =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt "\n") (pp_edge n) fmt l

let pp_node fmt (n : 'a node) =
  Fmt.pf fmt {|%a [label="%a"]; %a|} Fmt.int n.ind pp_exp (List.rev n.info)
    pp_edges (n.ind, n.children)

let pp_nodes fmt (g : 'a t) =
  Fmt.array ~sep:(fun fmt () -> Fmt.pf fmt "\n") pp_node fmt g

let pp_graph fmt (g : 'a t) =
  Fmt.pf fmt "digraph cfg {\n rankdir=LR;\n node [shape=record];\n %a}" pp_nodes
    g

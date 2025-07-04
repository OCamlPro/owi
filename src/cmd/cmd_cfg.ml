open Syntax
open Types

type to_add =
  | Ind of int
  | Next

let update_edges x next (edges, acc) (n, m, s) =
  match m with
  | Next -> ((n, next, s) :: edges, acc)
  | Ind 0 -> ((n, x, s) :: edges, acc)
  | Ind m -> (edges, (n, Ind (m - 1), s) :: acc)

let increase x =
  let n, m, s = x in
  match m with Ind m -> (n, Ind (m + 1), s) | _ -> x

let rec build_graph (l : binary expr) nodes n node edges
  (edges_to_add : (int * to_add * string option) list) =
  match l with
  | [] ->
    let nodes = (n, node) :: nodes in
    let edges_to_add = (n, Next, None) :: edges_to_add in
    (nodes, edges, n + 1, edges_to_add)
  | instr :: l -> (
    match instr.raw with
    | Block (_, _, exp) ->
      let nodes, edges, n, edges_to_add =
        build_graph exp.raw nodes n node edges (List.map increase edges_to_add)
      in
      let edges, edges_to_add =
        List.fold_left (update_edges n n) (edges, []) edges_to_add
      in
      build_graph l nodes n [] edges edges_to_add
    | Loop (_, _, exp) ->
      let nodes = (n, instr :: node) :: nodes in
      let edges = (n, n + 1, None) :: edges in
      let nodes, edges, n', edges_to_add =
        build_graph exp.raw nodes (n + 1) [] edges
          (List.map increase edges_to_add)
      in
      let edges, edges_to_add =
        List.fold_left (update_edges n n') (edges, []) edges_to_add
      in
      build_graph l nodes n' [] edges edges_to_add
    | If_else (_, _, e1, e2) ->
      let nodes = (n, instr :: node) :: nodes in

      let edges = (n, n + 1, Some "true") :: edges in
      let nodes, edges, n1, edges_to_add =
        build_graph e1.raw nodes (n + 1) [] edges
          (List.map increase edges_to_add)
      in

      let edges = (n, n1, Some "false") :: edges in
      let nodes, edges, n2, edges_to_add' =
        build_graph e2.raw nodes n1 [] edges []
      in

      let edges, edges_to_add =
        List.fold_left (update_edges n2 n2) (edges, [])
          (edges_to_add' @ edges_to_add)
      in
      build_graph l nodes n2 [] edges edges_to_add
    | Br (Raw i) ->
      let nodes = (n, instr :: node) :: nodes in
      let edges_to_add = (n, Ind i, None) :: edges_to_add in
      (nodes, edges, n + 1, edges_to_add)
    | Br_if (Raw i) ->
      let nodes = (n, instr :: node) :: nodes in
      let edges_to_add = (n, Ind i, Some "true") :: edges_to_add in
      let edges = (n, n + 1, Some "false") :: edges in
      build_graph l nodes (n + 1) [] edges edges_to_add
    | Return ->
      let nodes = (n, instr :: node) :: nodes in
      (nodes, edges, n + 1, edges_to_add)
    | _ -> build_graph l nodes n (instr :: node) edges edges_to_add )

let build_cfg instr =
  let nodes, edges, _, _ = build_graph instr [] 0 [] [] [] in
  (nodes, edges)

let pp_inst fmt i = Fmt.pf fmt "%a" (pp_instr ~short:true) i.Annotated.raw

let pp_exp fmt l = Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt " | ") pp_inst fmt l

let pp_node fmt (n, exp) =
  Fmt.pf fmt {|%a [label="%a"]|} Fmt.int n pp_exp (List.rev exp)

let pp_nodes fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";\n") pp_node fmt l

let pp_label fmt s = Fmt.pf fmt {|[label="%a"]|} Fmt.string s

let pp_label_opt fmt s_opt = Fmt.option pp_label fmt s_opt

let pp_edge fmt (n1, n2, s) =
  Fmt.pf fmt "%a -> %a %a" Fmt.int n1 Fmt.int n2 pp_label_opt s

let pp_edges fmt l =
  Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ";\n") pp_edge fmt l

let pp_graph fmt (nodes, edges) =
  Fmt.pf fmt "digraph cfg {\n rankdir=LR;\n node [shape=record];\n %a;\n %a}"
    pp_nodes nodes pp_edges edges

let cmd ~source_file =
  let* m =
    Compile.File.until_validate ~unsafe:false ~rac:false ~srac:false source_file
  in
  let f =
    match Array.get m.func 0 with Runtime.Local f -> f | _ -> assert false
  in
  (* add a parameter later *)
  let nodes, edges = build_cfg f.body.raw in
  Logs.app (fun log -> log "%a" pp_graph (List.rev nodes, List.rev edges));
  let* () =
    Bos.OS.File.writef (Fpath.v "cfg.dot") "%a" pp_graph
      (List.rev nodes, List.rev edges)
  in
  Ok ()

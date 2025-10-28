open Syntax
open Types

type to_add =
  | Ind of int
  | Next
  | End

let update_edges x next (edges, acc) (n, m, s) =
  match m with
  | Next -> ((n, next, s) :: edges, acc)
  | Ind 0 -> ((n, x, s) :: edges, acc)
  | Ind m -> (edges, (n, Ind (m - 1), s) :: acc)
  | _ -> (edges, (n, m, s) :: acc)

let update_edges_end x edges (n, m, s) =
  match m with End -> (n, x, s) :: edges | _ -> edges

let increase x =
  let n, m, s = x in
  match m with Ind m -> (n, Ind (m + 1), s) | _ -> x

let rec build_graph (l : binary expr) nodes n node edges
  (edges_to_add : (int * to_add * string option) list) continue =
  match l with
  | [] -> (
    match node with
    | [] ->
      if continue then
        let nodes = (n, node) :: nodes in
        let edges_to_add = (n, Next, None) :: edges_to_add in
        (nodes, edges, n + 1, edges_to_add, true)
      else (nodes, edges, n, edges_to_add, true)
    | _ ->
      let nodes = (n, node) :: nodes in
      let edges_to_add = (n, Next, None) :: edges_to_add in
      (nodes, edges, n + 1, edges_to_add, true) )
  | instr :: l -> (
    match instr.raw with
    | Block (_, _, exp) ->
      let nodes, edges, n, edges_to_add, continue =
        build_graph exp.raw nodes n node edges
          (List.map increase edges_to_add)
          continue
      in
      let edges, edges_to_add =
        List.fold_left (update_edges n n) (edges, []) edges_to_add
      in
      build_graph l nodes n [] edges edges_to_add continue
    | Loop (_, _, exp) ->
      let nodes, edges, n =
        match node with
        | [] -> (nodes, edges, n)
        | _ ->
          let nodes = (n, node) :: nodes in
          let edges = (n, n + 1, None) :: edges in
          (nodes, edges, n + 1)
      in
      let nodes, edges, n', edges_to_add, continue =
        build_graph exp.raw nodes n [ instr ] edges
          (List.map increase edges_to_add)
          continue
      in
      let edges, edges_to_add =
        List.fold_left (update_edges n n') (edges, []) edges_to_add
      in
      build_graph l nodes n' [] edges edges_to_add continue
    | If_else (_, _, e1, e2) ->
      let nodes = (n, instr :: node) :: nodes in

      let edges = (n, n + 1, Some "true") :: edges in
      let nodes, edges, n1, edges_to_add, continue' =
        build_graph e1.raw nodes (n + 1) [] edges
          (List.map increase edges_to_add)
          continue
      in

      let edges = (n, n1, Some "false") :: edges in
      let nodes, edges, n2, edges_to_add', continue =
        build_graph e2.raw nodes n1 [] edges [] continue
      in

      let edges, edges_to_add =
        List.fold_left (update_edges n2 n2) (edges, [])
          (edges_to_add' @ edges_to_add)
      in
      build_graph l nodes n2 [] edges edges_to_add (continue || continue')
    | Br (Raw i) ->
      let nodes = (n, instr :: node) :: nodes in
      let edges_to_add = (n, Ind i, None) :: edges_to_add in
      (nodes, edges, n + 1, edges_to_add, false)
    | Br_if (Raw i) ->
      let nodes = (n, instr :: node) :: nodes in
      let edges_to_add = (n, Ind i, Some "true") :: edges_to_add in
      let edges = (n, n + 1, Some "false") :: edges in
      build_graph l nodes (n + 1) [] edges edges_to_add continue
    | Br_table (inds, Raw i) ->
      let nodes = (n, instr :: node) :: nodes in
      let edges_to_add = (n, Ind i, Some "default") :: edges_to_add in
      let edges_to_add, _ =
        Array.fold_left
          (fun (acc, x) (Raw i : binary indice) ->
            ((n, Ind i, Some (string_of_int x)) :: acc, x + 1) )
          (edges_to_add, 0) inds
      in
      (nodes, edges, n + 1, edges_to_add, false)
    | Return | Return_call _ | Return_call_indirect _ | Return_call_ref _ ->
      let nodes = (n, node) :: nodes in
      let edges_to_add = (n, End, None) :: edges_to_add in
      (nodes, edges, n + 1, edges_to_add, false)
    | Unreachable ->
      let nodes = (n, instr :: node) :: nodes in
      (nodes, edges, n + 1, edges_to_add, false)
    | Call _ | Call_indirect _ ->
      let nodes = (n, instr :: node) :: nodes in
      let edges = (n, n + 1, None) :: edges in
      build_graph l nodes (n + 1) [] edges edges_to_add continue
    | _ -> build_graph l nodes n (instr :: node) edges edges_to_add continue )

let build_cfg instr =
  let nodes, edges, n, edges_to_add, _ = build_graph instr [] 0 [] [] [] true in
  let nodes = (n, [ Annotated.dummy Return ]) :: nodes in
  let edges, edges_to_add =
    List.fold_left (update_edges n n) (edges, []) edges_to_add
  in
  let edges = List.fold_left (update_edges_end n) edges edges_to_add in
  (nodes, edges)

let build_cfg_from_text_module modul entry =
  let m =
    Compile.Text.until_validate ~unsafe:false ~rac:false ~srac:false modul
  in
  match m with
  | Ok m ->
    let f =
      match Array.get m.func entry with
      | Runtime.Local f -> f
      | _ -> assert false
    in
    let nodes, edges = build_cfg f.body.raw in
    Graph.init_cfg nodes edges
  | _ -> assert false

let build_cfg_from_func f =
  let nodes, edges = build_cfg f.body.raw in
  Graph.init_cfg nodes edges

let cmd ~source_file ~entry_point ~scc =
  let* m =
    Compile.File.until_validate ~unsafe:false ~rac:false ~srac:false source_file
  in
  let entry =
    Option.value
      (Option.bind entry_point (fun x ->
         match int_of_string_opt x with
         | Some _ as x -> x
         | None ->
           Array.find_index
             (function
               | Runtime.Local y ->
                 Option.compare String.compare (Some x) y.id = 0
               | _ -> false )
             m.func ) )
      ~default:0
  in
  let f =
    match Array.get m.func entry with Runtime.Local f -> f | _ -> assert false
  in
  let nodes, edges = build_cfg f.body.raw in
  let graph = Graph.init_cfg nodes edges in
  if scc then
    let scc = Graph.build_scc_graph graph in

    Bos.OS.File.writef
      (Fpath.set_ext ".dot" source_file)
      "%a" Graph.pp_scc_graph (scc, Graph.Cfg)
  else
    Bos.OS.File.writef
      (Fpath.set_ext ".dot" source_file)
      "%a" Graph.pp_cfg graph

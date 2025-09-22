module Target = struct
  type t =
    { func_idx : int
    ; block_idx : int
    }
end

module type Score = sig
  type t

  val empty : unit -> t

  val add : t -> Target.t -> int -> unit

  val get : t -> Target.t -> int option
end

module Score : Score = struct
  type t = (Target.t, int) Hashtbl.t

  let empty () = Hashtbl.create 256

  let add score target x = Hashtbl.add score target x

  let get score target = Hashtbl.find_opt score target
end

let compute_score_on_cg (_g : Call_graph.t) = assert false

let compute_distance_to_unreachable _ =
  let ignore _ = () in
  let _ = ignore (module Score : Score) in
  let _ = ignore compute_score_on_cg in
  let { Target.func_idx; block_idx } = { Target.func_idx = 0; block_idx = 0 } in
  ignore func_idx;
  ignore block_idx;
  assert false

(*

(**************************** CFG *)

let set_instr_distances (g : Control_flow_graph.t)
  (vertex : Control_flow_graph.Vertex.t) (d : distances) cg =
  match vertex.expr with
  | [] -> ()
  | h :: _ -> (
    match h.Annotated.raw with
    | Types.(If_else _ | Br_if _) -> begin
      match Control_flow_graph.succ g vertex with
      | [ (t, Some "true"); (f, Some "false") ]
      | [ (f, Some "false"); (t, Some "true") ] ->
        Annotated.set_d_true h d.(cg).(t);
        Annotated.set_d_false h d.(cg).(f)
      | _ -> assert false
    end
    | _ -> () )

let rec cfg_distance_target (cfg : Control_flow_graph.t) d
  (distances : distances) target_idx func_idx block_idx =
  let node = cfg.Custom_graph.nodes.(block_idx) in
  let d =
    match node.Custom_graph.children with [] | [ _ ] -> d | _ -> d + 1
  in
  let d' = distances.(func_idx).(block_idx).(target_idx) in
  if d < d' then (
    distances.(func_idx).(block_idx).(target_idx) <- d;
    set_instr_distances node distances func_idx;
    List.iter
      (cfg_distance_target cfg d distances target_idx func_idx)
      node.parents )

let cfg_find_targets func_idx acc calls (cfg : Control_flow_graph.t) =
  Control_flow_graph.fold_vertex
    (fun vertex (acc, calls) ->
      match vertex.expr with
      | { Annotated.raw = Types.Unreachable; _ } :: _ ->
        if Control_flow_graph.in_degree cfg vertex = 0 then begin
          if vertex.idx = 0 then ((func_idx, 0) :: acc, calls) else (acc, calls)
        end
        else ((func_idx, vertex.idx) :: acc, calls)
      | { Annotated.raw =
            Types.(
              Call _ | Call_indirect _ | Return_call _ | Return_call_indirect _)
        ; Annotated.functions_called = funcs
        ; _
        }
        :: _ ->
        let calls =
          IntSet.fold
            (fun f acc -> Calls.add_to_list (func_idx, f) vertex.idx acc)
            funcs calls
        in
        (acc, calls)
      | _ -> (acc, calls) )
    cfg (acc, calls)

(* TODO: parameterize the target *)
let cg_find_targets (cg : Call_graph.t) =
  Call_graph.fold_vertex
    (fun node (acc, calls) ->
      match node with
      | Outside_world | Function { cfg = None; _ } -> (acc, calls)
      | Function { cfg = Some cfg; idx } -> cfg_find_targets idx acc calls cfg )
    cg ([], Calls.empty)

(***************************** CG *)

let rec compute_distance_to_target (cg : Call_graph.t) calls d
  (distances : distances) target_idx (func_idx, block_idx) =
  Call_graph.iter_vertex
    (function
      | Outside_world | Function { cfg = None; _ } -> ()
      | Function { cfg = Some _; idx } when idx <> func_idx -> ()
      | Function { cfg = Some cfg; idx } as vertex ->
        assert (idx = func_idx);
        cfg_distance_target cfg d distances target_idx func_idx block_idx;
        Call_graph.iter_pred
          (function
            | Outside_world | Function { cfg = None; _ } -> ()
            | Function { cfg = Some _; idx = p_idx } -> (
              match Calls.find_opt (p_idx, func_idx) calls with
              | Some l ->
                List.iter
                  (fun block_idx ->
                    let d =
                      match Hashtbl.find_opt distances func_idx with
                      | None -> assert false
                      | Some matrix -> matrix.(0).(target_idx)
                    in
                    let d' =
                      match Hashtbl.find_opt distances p_idx with
                      | None -> assert false
                      | Some matrix -> matrix.(block_idx).(target_idx)
                    in
                    if d < d' then
                      compute_distance_to_target cg calls d distances target_idx
                        (p_idx, block_idx) )
                  l
              | None -> () ) )
          cg vertex )
    cg

let compute_distance_to_targets (cg : Call_graph.t) =
  let targets, calls = cg_find_targets cg in
  let distances : distances =
    let tbl = Hashtbl.create 64 in
    Call_graph.iter_vertex
      (function
        | Call_graph.Vertex.Outside_world | Function { cfg = None; _ } -> ()
        | Function { cfg = Some cfg; idx = func_idx } ->
          Control_flow_graph.iter_vertex
            (fun { idx = block_idx; _ } ->
              List.iter
                (fun target_idx ->
                  let target = { Target.func_idx; block_idx; target_idx } in
                  assert (not (Hashtbl.mem tbl target));
                  Hashtbl.add tbl target Int.max_int )
                targets )
            cfg )
      cg;
    tbl
  in
  List.iteri
    (fun i target -> compute_distance_to_target cg calls 0 distances i target)
    targets
    *)

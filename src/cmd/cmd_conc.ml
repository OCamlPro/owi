(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Bos
open Syntax
module Choice = Concolic.Choice

(* let () = Random.self_init () *)
let () = Random.init 42

(* TODO: add a flag for this *)
let print_paths = false

let ( let** ) (t : 'a Result.t Choice.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  Choice.bind t (fun t ->
    match t with Error e -> Choice.return (Error e) | Ok x -> f x )

let simplify_then_link ~entry_point ~invoke_with_symbols ~unsafe ~rac ~srac
  ~optimize link_state m =
  let* m =
    match m with
    | Kind.Wat _ | Wasm _ ->
      Compile.Any.until_binary_validate ~unsafe ~rac ~srac m
    | Wast _ -> Fmt.error_msg "can't run concolic interpreter on a script"
    | Ocaml _ -> assert false
  in
  let* m = Cmd_utils.set_entry_point entry_point invoke_with_symbols m in
  let+ m, link_state =
    Compile.Binary.until_link ~unsafe ~optimize ~name:None link_state m
  in
  let module_to_run = Concolic.convert_module_to_run m in
  (link_state, module_to_run)

let simplify_then_link_files ~entry_point ~invoke_with_symbols ~unsafe ~rac
  ~srac ~optimize filenames =
  let link_state = Link.empty_state in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic"
      ~func_typ:Concolic.Extern_func.extern_type
      Concolic_wasm_ffi.symbolic_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"summaries"
      ~func_typ:Concolic.Extern_func.extern_type
      Concolic_wasm_ffi.summaries_extern_module
  in
  let+ link_state, modules_to_run =
    List.fold_left
      (fun (acc : (_ * _) Result.t) filename ->
        let* link_state, modules_to_run = acc in
        let* m0dule = Parse.guess_from_file filename in
        let+ link_state, module_to_run =
          simplify_then_link ~entry_point ~invoke_with_symbols ~unsafe ~rac
            ~srac ~optimize link_state m0dule
        in
        (link_state, module_to_run :: modules_to_run) )
      (Ok (link_state, []))
      filenames
  in
  (link_state, List.rev modules_to_run)

let run_modules_to_run (link_state : _ Link.state) modules_to_run =
  List.fold_left
    (fun (acc : unit Result.t Concolic.Choice.t) to_run ->
      let** () = acc in
      (Interpret.Concolic.modul link_state.envs) to_run )
    (Choice.return (Ok ())) modules_to_run

type end_of_trace =
  | Assume_fail
  | Assert_fail
  | Trap of Trap.t
  | Normal

type trace =
  { assignments : Concolic_choice.assignments
  ; remaining_pc : Concolic_choice.pc_elt list
  ; end_of_trace : end_of_trace
  }

module IMap = Map.Make (Prelude.Int32)

module Unexplored : sig
  type t

  val none : t -> bool

  val zero : t

  val one : t

  val add : t -> t -> t
end = struct
  type t = int

  let none t = t = 0

  let zero = 0

  let one = 1

  let add a b = a + b
end

type unexplored = Unexplored.t

type assert_status =
  | Unknown
  | Valid
  | Invalid of Concolic_choice.assignments

type node =
  | Select of
      { cond : Smtml.Expr.t
      ; if_true : eval_tree
      ; if_false : eval_tree
      }
  | Select_i32 of
      { value : Smtml.Expr.t
      ; branches : eval_tree IMap.t
      }
  | Assume of
      { cond : Smtml.Expr.t
      ; cont : eval_tree
      }
  | Assert of
      { cond : Smtml.Expr.t
      ; cont : eval_tree
      ; mutable status : assert_status
      }
  | Unreachable
  | Not_explored  (** A node was discovered but has not yet been explored *)
  | Being_explored
    (** A node that has been seen in `find_node_to_run` and is being explored *)
  | Explored  (** A fully explored path *)

and eval_tree =
  { mutable node : node
  ; mutable unexplored : unexplored
  ; pc : Concolic_choice.pc
  ; mutable ends : (end_of_trace * Concolic_choice.assignments) list
  }

let pp_node fmt = function
  | Select _ -> Fmt.string fmt "Select"
  | Select_i32 _ -> Fmt.string fmt "Select_i32"
  | Assume _ -> Fmt.string fmt "Assume"
  | Assert _ -> Fmt.string fmt "Assert"
  | Unreachable -> Fmt.string fmt "Unreachable"
  | Not_explored -> Fmt.string fmt "Not_explored"
  | Being_explored -> Fmt.string fmt "Being_explored"
  | Explored -> Fmt.string fmt "Explored"

let _ = pp_node

let count_unexplored (tree : eval_tree) : Unexplored.t =
  match tree.node with
  | Select { if_true; if_false; _ } ->
    Unexplored.add if_true.unexplored if_false.unexplored
  | Select_i32 { branches; _ } ->
    IMap.fold
      (fun _ branch -> Unexplored.add branch.unexplored)
      branches Unexplored.zero
  | Assume { cont; _ } | Assert { cont; _ } -> cont.unexplored
  | Unreachable | Being_explored | Explored -> Unexplored.zero
  | Not_explored -> Unexplored.one

let update_unexplored tree = tree.unexplored <- count_unexplored tree

let update_node (tree : eval_tree) (node : node) : unit =
  tree.node <- node;
  update_unexplored tree

let fresh_tree (pc : Concolic_choice.pc) : eval_tree =
  { node = Not_explored; unexplored = Unexplored.one; pc; ends = [] }

let new_node (pc : Concolic_choice.pc) (head : Concolic_choice.pc_elt) : node =
  match head with
  | Select (_, cond) ->
    Select
      { cond
      ; if_true = fresh_tree (Select (true, cond) :: pc)
      ; if_false = fresh_tree (Select (false, cond) :: pc)
      }
  | Select_i32 (_, value) -> Select_i32 { value; branches = IMap.empty }
  | Assume cond -> Assume { cond; cont = fresh_tree (Assume cond :: pc) }
  | Assert cond ->
    Assert { cond; cont = fresh_tree (Assert cond :: pc); status = Unknown }
  | EltExplicitStop -> Explored

let try_initialize (pc : Concolic_choice.pc) (node : eval_tree)
  (head : Concolic_choice.pc_elt) =
  match node.node with
  | Not_explored | Being_explored -> update_node node (new_node pc head)
  | _ -> ()

let check = true

let set_on_unexplored tree transition =
  match tree.node with
  | Not_explored | Being_explored -> tree.node <- transition
  | _ -> assert false (* Sanity Check *)

let rec add_trace (pc : Concolic_choice.pc) (node : eval_tree) (trace : trace)
  (to_update : eval_tree list) : eval_tree list =
  match trace.remaining_pc with
  | [] -> begin
    node.ends <- (trace.end_of_trace, trace.assignments) :: node.ends;
    ( match trace.end_of_trace with
    | Normal -> set_on_unexplored node Explored
    | Assume_fail ->
      (* TODO: do something in this case? Otherwise, it might remain in Being_explored in unsat paths? *)
      ()
    | Trap Unreachable -> set_on_unexplored node Unreachable
    | Trap _ | Assert_fail ->
      (* TODO: Mark this as explored to avoid possibly empty pcs? *)
      () );
    node :: to_update
  end
  | head_of_trace :: tail_of_trace -> (
    try_initialize pc node head_of_trace;
    let pc = head_of_trace :: pc in
    match (node.node, head_of_trace) with
    | Assert _, EltExplicitStop -> assert false
    | (Unreachable | Not_explored | Being_explored | Explored), _ ->
      assert false
    | Select { cond; if_true; if_false }, Select (v, cond') ->
      if check then assert (Smtml.Expr.equal cond cond');
      let branch = if v then if_true else if_false in
      add_trace pc branch
        { trace with remaining_pc = tail_of_trace }
        (node :: to_update)
    | _, Select _ | Select _, _ -> assert false
    | Select_i32 { value; branches }, Select_i32 (v, value') ->
      if check then assert (Smtml.Expr.equal value value');
      let branch =
        match IMap.find_opt v branches with
        | None ->
          let t = fresh_tree pc in
          update_node node
            (Select_i32 { value; branches = IMap.add v t branches });
          t
        | Some t -> t
      in
      add_trace pc branch
        { trace with remaining_pc = tail_of_trace }
        (node :: to_update)
    | _, Select_i32 _ | Select_i32 _, _ -> assert false
    | Assume { cond; cont }, Assume cond' ->
      if check then assert (Smtml.Expr.equal cond cond');
      add_trace pc cont
        { trace with remaining_pc = tail_of_trace }
        (node :: to_update)
    | _, Assume _ | Assume _, _ -> assert false
    | Assert ({ cond; cont; status = _ } as assert_), Assert cond' ->
      if check then assert (Smtml.Expr.equal cond cond');
      ( match (tail_of_trace, trace.end_of_trace) with
      | [], Assert_fail -> assert_.status <- Invalid trace.assignments
      | _ -> () );
      add_trace pc cont
        { trace with remaining_pc = tail_of_trace }
        (node :: to_update) )

let add_trace tree trace =
  let to_update = add_trace [] tree trace [] in
  List.iter update_unexplored to_update

let run_once link_state modules_to_run (forced_values : Smtml.Model.t) =
  let backups = List.map Concolic.backup modules_to_run in
  let result = run_modules_to_run link_state modules_to_run in
  let ( result
      , Concolic_choice.
          { pc
          ; symbols = _
          ; symbols_value
          ; shared = _
          ; preallocated_values = _
          } ) =
    Concolic_choice.run forced_values result
  in
  let () = List.iter2 Concolic.recover backups modules_to_run in
  let+ end_of_trace =
    match result with
    | Ok (Ok ()) -> Ok Normal
    | Ok (Error _ as e) -> e
    | Error (Trap t) -> Ok (Trap t)
    | Error Assert_fail -> Ok Assert_fail
    | Error (Assume_fail _c) -> Ok Assume_fail
    | Error ErrExplicitStop -> Ok Normal
  in
  let trace =
    { assignments = symbols_value; remaining_pc = List.rev pc; end_of_trace }
  in
  (result, trace)

(* Very naive ! *)
let find_node_to_run tree =
  let ( let* ) = Option.bind in
  let rec loop tree to_update =
    match tree.node with
    | Not_explored ->
      Log.debug2 "Try unexplored@.%a@." Concolic_choice.pp_pc tree.pc;
      Some (tree.pc, tree :: to_update)
    | Select { cond = _; if_true; if_false } ->
      let* b =
        match
          ( Unexplored.none if_true.unexplored
          , Unexplored.none if_false.unexplored )
        with
        | true, false -> Some false (* Unexplored paths in `else` branch *)
        | false, true -> Some true (* Unexplored paths in `then` branch *)
        | false, false -> Some (Random.bool ()) (* Unexplored in both *)
        | true, true -> None (* No unexplored remain *)
      in
      Log.debug1 "Select bool %b@." b;
      loop (if b then if_true else if_false) (tree :: to_update)
    | Select_i32 { value = _; branches } ->
      (* TODO: better ! *)
      let branches = IMap.bindings branches in
      let branches =
        List.filter (fun (_i, v) -> not (Unexplored.none v.unexplored)) branches
      in
      let n = List.length branches in
      if n = 0 then None
      else begin
        let i = Random.int n in
        (* TODO: change the datastructure or stop being random ? *)
        match List.nth_opt branches i with
        | None -> assert false
        | Some (i, branch) ->
          Log.debug1 "Select_i32 %li@." i;
          loop branch (tree :: to_update)
      end
    | Assume { cond = _; cont } -> loop cont (tree :: to_update)
    | Assert { cond; cont = _; status = Unknown } ->
      let pc : Concolic_choice.pc = Select (false, cond) :: tree.pc in
      Log.debug2 "Try Assert@.%a@.@." Concolic_choice.pp_pc pc;
      Some (pc, tree :: to_update)
    | Assert { cond = _; cont; status = _ } -> loop cont (tree :: to_update)
    | Unreachable ->
      Log.debug2 "Unreachable (Retry)@.%a@." Concolic_choice.pp_pc tree.pc;
      None
    | Being_explored | Explored -> None
  in
  loop tree []

let pc_model solver pc =
  let pc = Concolic_choice.pc_to_exprs pc in
  match Solver.check solver pc with
  | `Unsat | `Unknown -> None
  | `Sat ->
    let symbols = Some (Smtml.Expr.get_symbols pc) in
    Some (Solver.model ~symbols ~pc solver)

let rec find_model_to_run solver tree =
  let ( let* ) = Option.bind in
  let* pc, to_update = find_node_to_run tree in
  let node = match to_update with [] -> assert false | hd :: _tl -> hd in
  let model =
    match pc with
    | [] -> assert false
    | pc -> (
      match pc_model solver pc with
      | None ->
        ( match node.node with
        | Not_explored -> node.node <- Unreachable
        | Assert assert_ -> assert_.status <- Valid
        | _ -> assert false (* Sanity check *) );
        find_model_to_run solver tree
      | Some model ->
        ( match node.node with
        | Not_explored -> node.node <- Being_explored
        | Assert assert_ -> assert_.status <- Invalid [] (* TODO: assignments *)
        | _ -> assert false (* Sanity check *) );
        Some model )
  in
  (* Have to update tree because of the paths trimmed above *)
  List.iter update_unexplored to_update;
  model

let count_path = ref 0

let run solver tree link_state modules_to_run =
  (* Initial model is empty (no symbolic variables yet) *)
  let initial_model = Hashtbl.create 0 in
  let rec loop model =
    incr count_path;
    let* result, trace = run_once link_state modules_to_run model in
    add_trace tree trace;
    let* error =
      match result with
      | Ok (Ok ()) -> Ok None
      | Ok (Error _ as e) -> e
      | Error (Assume_fail c) -> (
        decr count_path;
        (* Current path condition led to assume failure, try to satisfy *)
        Log.debug2 "Assume_fail: %a@." Smtml.Expr.pp c;
        Log.debug2 "Pc:@.%a" Concolic_choice.pp_pc trace.remaining_pc;
        Log.debug2 "Assignments:@.%a@."
          (Concolic_choice.pp_assignments ~no_value:false)
          trace.assignments;
        Log.debug0 "Retry !@.";
        match pc_model solver (Assume c :: trace.remaining_pc) with
        | Some model -> loop model
        | None -> Ok None )
      | Error (Trap trap) ->
        (* TODO: Check if we want to report this *)
        Ok (Some (`Trap trap, trace))
      | Error Assert_fail ->
        (* TODO: Check if we want to report this *)
        Ok (Some (`Assert_fail, trace))
      | Error ErrExplicitStop -> Ok None
    in
    match error with
    | Some _ -> Ok error
    | None -> (
      (* No error, we can go again if we still have stuff to explore *)
      match find_model_to_run solver tree with
      | None -> Ok None
      | Some model -> loop model )
  in
  loop initial_model

let assignments_to_model (assignments : (Smtml.Symbol.t * V.t) list) :
  Smtml.Model.t =
  let table = Hashtbl.create (List.length assignments) in
  List.iter
    (fun (s, v) ->
      let value =
        match v with
        | Concrete_value.I32 x -> Smtml.Value.Num (I32 x)
        | I64 x -> Num (I64 x)
        | F32 x -> Num (F32 (Float32.to_bits x))
        | F64 x -> Num (F64 (Float64.to_bits x))
        | Ref _ -> assert false
      in
      Hashtbl.add table s value )
    assignments;
  table

(* NB: This function propagates potential errors (Result.err) occurring
   during evaluation (OS, syntax error, etc.), except for Trap and Assert,
   which are handled here. Most of the computations are done in the Result
   monad, hence the let*. *)
let cmd ~profiling ~debug ~unsafe ~rac ~srac ~optimize ~workers:_
  ~no_stop_at_failure:_ ~no_value ~no_assert_failure_expression_printing
  ~deterministic_result_order:_ ~fail_mode:_ ~workspace ~solver ~files ~profile
  ~model_format ~entry_point ~invoke_with_symbols ~model_out_file
  ~with_breadcrumbs:_ =
  let* workspace =
    match workspace with
    | Some path -> Ok path
    | None -> OS.Dir.tmp "owi_conc_%s"
  in
  let* _did_create : bool = OS.Dir.create Fpath.(workspace / "test-suite") in

  Option.iter Stats.init_logger_to_file profile;
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  (* deterministic_result_order implies no_stop_at_failure *)
  (* let no_stop_at_failure = deterministic_result_order || no_stop_at_failure in *)
  let to_string m =
    let model = assignments_to_model m in
    match model_format with
    | Cmd_utils.Json -> Smtml.Model.to_json_string model
    | Scfg -> Smtml.Model.to_scfg_string ~no_value model
  in
  let solver = Solver.fresh solver () in
  let* link_state, modules_to_run =
    simplify_then_link_files ~entry_point ~invoke_with_symbols ~unsafe ~rac
      ~srac ~optimize files
  in
  let tree = fresh_tree [] in
  let* result = run solver tree link_state modules_to_run in
  let testcase assignments =
    if not no_value then
      let testcase : Smtml.Value.t list =
        List.map
          (fun (_, v) ->
            match v with
            | Concrete_value.I32 x -> Smtml.Value.Num (I32 x)
            | I64 x -> Num (I64 x)
            | F32 x -> Num (F32 (Float32.to_bits x))
            | F64 x -> Num (F64 (Float64.to_bits x))
            | Ref _ -> assert false )
          assignments
      in
      Cmd_utils.write_testcase ~dir:Fpath.(workspace / "test-suite") testcase
    else Ok ()
  in
  if print_paths then Fmt.pr "Completed paths: %d@." !count_path;
  let to_file path assignements =
    Bos.OS.File.write path (to_string assignements)
  in
  match result with
  | None ->
    Fmt.pr "All OK@.";
    Ok ()
  | Some (`Trap trap, { assignments; _ }) ->
    let assignments = List.rev assignments in
    Fmt.pr "Trap: %s@\n" (Trap.to_string trap);
    let* () =
      match model_out_file with
      | Some path -> to_file path assignments
      | None -> Ok (Fmt.pr "Model:@\n @[<v>%s@]@." (to_string assignments))
    in
    let* () = testcase assignments in
    Error (`Found_bug 1)
  | Some (`Assert_fail, { assignments; _ }) ->
    let assignments = List.rev assignments in
    if no_assert_failure_expression_printing then begin
      Fmt.pr "Assert failure@\n"
    end
    else begin
      (* TODO: print the assert failure expression ! *)
      Fmt.pr "Assert failure@\n"
    end;
    let* () =
      match model_out_file with
      | Some path -> to_file path assignments
      | None -> Ok (Fmt.pr "Model:@\n @[<v>%s@]@." (to_string assignments))
    in
    let* () = testcase assignments in
    Error (`Found_bug 1)

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Choice = Concolic.P.Choice

(* let () = Random.self_init () *)
let () = Random.init 42

let debug = false

let ( let** ) (t : 'a Result.t Choice.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  Choice.bind t (fun t ->
      match t with Error e -> Choice.return (Error e) | Ok x -> f x )

let simplify_then_link ~unsafe ~optimize link_state m =
  let* m =
    match m with
    | Either.Left (Either.Left text_module) ->
      Compile.Text.until_binary ~unsafe text_module
    | Either.Left (Either.Right _text_scrpt) ->
      Error (`Msg "can't run concolic interpreter on a script")
    | Either.Right binary_module -> Ok binary_module
  in
  let* m = Cmd_utils.add_main_as_start m in
  let+ m, link_state =
    Compile.Binary.until_link ~unsafe link_state ~optimize ~name:None m
  in
  let module_to_run = Concolic.convert_module_to_run m in
  (link_state, module_to_run)

let simplify_then_link_files ~unsafe ~optimize filenames =
  let link_state = Link.empty_state in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic"
      ~func_typ:Concolic.P.Extern_func.extern_type
      Concolic_wasm_ffi.symbolic_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"summaries"
      ~func_typ:Concolic.P.Extern_func.extern_type
      Concolic_wasm_ffi.summaries_extern_module
  in
  let+ link_state, modules_to_run =
    List.fold_left
      (fun (acc : (_ * _) Result.t) filename ->
        let* link_state, modules_to_run = acc in
        let* m0dule = Parse.guess_from_file filename in
        let+ link_state, module_to_run =
          simplify_then_link ~unsafe ~optimize link_state m0dule
        in
        (link_state, module_to_run :: modules_to_run) )
      (Ok (link_state, []))
      filenames
  in
  (link_state, List.rev modules_to_run)

let run_modules_to_run (link_state : _ Link.state) modules_to_run =
  List.fold_left
    (fun (acc : unit Result.t Concolic.P.Choice.t) to_run ->
      let** () = acc in
      (Interpret.Concolic.modul link_state.envs) to_run )
    (Choice.return (Ok ())) modules_to_run

let get_model ~symbols solver pc =
  let pc = Concolic_choice.pc_to_exprs pc in
  Solver.model ~symbols ~pc solver

type assignments = (Smtml.Symbol.t * Concrete_value.t) list

type end_of_trace =
  | Assume_fail
  | Assert_fail
  | Trap of Trap.t
  | Normal

type trace =
  { assignments : assignments
  ; remaining_pc : Concolic_choice.pc_elt list
  ; end_of_trace : end_of_trace
  }

module IMap = Map.Make (Stdlib.Int32)

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
      ; mutable disproved : assignments option
      }
  | Unreachable
  | Not_explored

and eval_tree =
  { mutable node : node
  ; mutable unexplored : unexplored
  ; pc : Concolic_choice.pc
  ; mutable ends : (end_of_trace * assignments) list
  }

let rec rec_count_unexplored tree =
  match tree.node with
  | Select { if_true; if_false; _ } ->
    Unexplored.add
      (rec_count_unexplored if_true)
      (rec_count_unexplored if_false)
  | Select_i32 { branches; _ } ->
    IMap.fold
      (fun _ branch -> Unexplored.add (rec_count_unexplored branch))
      branches Unexplored.zero
  | Assume { cont; _ } | Assert { cont; _ } -> rec_count_unexplored cont
  | Unreachable -> Unexplored.zero
  | Not_explored -> Unexplored.one

let _ = rec_count_unexplored

let count_unexplored tree =
  match tree.node with
  | Select { if_true; if_false; _ } ->
    Unexplored.add if_true.unexplored if_false.unexplored
  | Select_i32 { branches; _ } ->
    IMap.fold
      (fun _ branch -> Unexplored.add branch.unexplored)
      branches Unexplored.zero
  | Assume { cont; _ } | Assert { cont; _ } -> cont.unexplored
  | Unreachable -> Unexplored.zero
  | Not_explored -> Unexplored.one

let update_unexplored tree = tree.unexplored <- count_unexplored tree

let update_node tree node =
  tree.node <- node;
  update_unexplored tree

let fresh_tree pc =
  { node = Not_explored; unexplored = Unexplored.one; pc; ends = [] }

let new_node pc (head : Concolic_choice.pc_elt) : node =
  match head with
  | Select (cond, _) ->
    Select
      { cond
      ; if_true = fresh_tree (Select (cond, true) :: pc)
      ; if_false = fresh_tree (Select (cond, false) :: pc)
      }
  | Select_i32 (value, _) -> Select_i32 { value; branches = IMap.empty }
  | Assume cond -> Assume { cond; cont = fresh_tree (Assume cond :: pc) }
  | Assert cond ->
    Assert { cond; cont = fresh_tree (Assert cond :: pc); disproved = None }

let try_initialize pc node head =
  match node.node with
  | Not_explored -> update_node node (new_node pc head)
  | _ -> ()

let check = true

let rec add_trace pc node (trace : trace) to_update : eval_tree list =
  match trace.remaining_pc with
  | [] -> begin
    node.ends <- (trace.end_of_trace, trace.assignments) :: node.ends;
    let () =
      match trace.end_of_trace with
      | Trap Unreachable -> begin
        match node.node with
        | Not_explored -> node.node <- Unreachable
        | Unreachable -> ()
        | _ -> assert false
      end
      | _ -> ()
    in
    node :: to_update
  end
  | head_of_trace :: tail_of_trace -> (
    try_initialize pc node head_of_trace;
    let pc = head_of_trace :: pc in
    match (node.node, head_of_trace) with
    | Not_explored, _ -> assert false
    | Unreachable, _ -> assert false
    | Select { cond; if_true; if_false }, Select (cond', v) ->
      if check then assert (Smtml.Expr.equal cond cond');
      let branch = if v then if_true else if_false in
      add_trace pc branch
        { trace with remaining_pc = tail_of_trace }
        (node :: to_update)
    | _, Select _ | Select _, _ -> assert false
    | Select_i32 { value; branches }, Select_i32 (value', v) ->
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
    | Assert ({ cond; cont; disproved = _ } as assert_), Assert cond' ->
      if check then assert (Smtml.Expr.equal cond cond');
      begin
        match (tail_of_trace, trace.end_of_trace) with
        | [], Assert_fail -> assert_.disproved <- Some trace.assignments
        | _ -> ()
      end;
      add_trace pc cont
        { trace with remaining_pc = tail_of_trace }
        (node :: to_update) )

let add_trace tree trace =
  let to_update = add_trace [] tree trace [] in
  List.iter update_unexplored to_update

let run_once tree link_state modules_to_run forced_values =
  let backups = List.map Concolic.backup modules_to_run in
  let result = run_modules_to_run link_state modules_to_run in
  let ( ( result
        , Choice.
            { pc
            ; symbols = _
            ; symbols_value
            ; shared = _
            ; preallocated_values = _
            } ) as r ) =
    let forced_values =
      match forced_values with None -> Hashtbl.create 0 | Some v -> v
    in
    Choice.run forced_values result
  in
  let () = List.iter2 Concolic.recover backups modules_to_run in
  let end_of_trace =
    match result with
    | Ok (Ok ()) -> Normal
    | Ok (Error e) -> Result.failwith e
    | Error (Trap t) -> Trap t
    | Error Assert_fail -> Assert_fail
    | Error (Assume_fail _c) -> Assume_fail
  in
  let trace =
    { assignments = symbols_value; remaining_pc = List.rev pc; end_of_trace }
  in
  if debug then begin
    Format.pp_std "Add trace:@\n";
    Format.pp_std "%a@\n" Concolic_choice.pp_pc trace.remaining_pc
  end;
  add_trace tree trace;
  r

(* Very naive ! *)
let rec find_node_to_run tree =
  match tree.node with
  | Not_explored ->
    if debug then begin
      Format.pp_std "Try unexplored@.%a@.@." Concolic_choice.pp_pc tree.pc
    end;
    Some tree.pc
  | Select { cond = _; if_true; if_false } ->
    let b =
      if Unexplored.none if_true.unexplored then false
      else if Unexplored.none if_false.unexplored then true
      else Random.bool ()
    in
    if debug then begin
      Format.pp_std "Select bool %b@." b
    end;
    let tree = if b then if_true else if_false in
    find_node_to_run tree
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
      let i, branch = List.nth branches i in
      if debug then begin
        Format.pp_std "Select_i32 %li@." i
      end;
      find_node_to_run branch
    end
  | Assume { cond = _; cont } -> find_node_to_run cont
  | Assert { cond; cont = _; disproved = None } ->
    let pc : Concolic_choice.pc = Select (cond, false) :: tree.pc in
    Format.pp_std "Try Assert@.%a@.@." Concolic_choice.pp_pc pc;
    Some pc
  | Assert { cond = _; cont; disproved = Some _ } -> find_node_to_run cont
  | Unreachable ->
    Format.pp_std "Unreachable (Retry)@.%a@." Concolic_choice.pp_pc tree.pc;
    None

let pc_model solver pc =
  let pc = Concolic_choice.pc_to_exprs pc in
  match Solver.check solver pc with
  | `Unsat | `Unknown -> None
  | `Sat ->
    let symbols = None in
    let model = Solver.model ~symbols ~pc solver in
    Some model

let find_model_to_run solver tree =
  match find_node_to_run tree with
  | None -> None
  | Some pc -> pc_model solver pc

let launch solver tree link_state modules_to_run =
  let rec find_model n =
    if n = 0 then begin
      Format.pp_std "Failed to find something to run@\n";
      None
    end
    else
      match find_model_to_run solver tree with
      | None -> find_model (n - 1)
      | Some m ->
        if debug then begin
          Format.pp_std "Found something to run %a@\n"
            (Smtml.Model.pp ~no_values:false)
            m
        end;
        Some m
  in
  let rec loop count =
    if count <= 0 then None
    else
      let model = find_model 20 in
      run_model model count
  and run_model model count =
    let r, thread = run_once tree link_state modules_to_run model in
    match r with
    | Ok (Ok ()) -> loop (count - 1)
    | Ok (Error e) -> Result.failwith e
    | Error (Assume_fail c) -> begin
      if debug then begin
        Format.pp_std "Assume_fail: %a@\n" Smtml.Expr.pp c;
        Format.pp_std "Assignments:@\n%a@\n" Concolic_choice.pp_assignments
          thread.symbols_value;
        Format.pp_std "Retry !@\n"
      end;
      match pc_model solver thread.pc with
      | None ->
        Format.pp_err "Can't satisfy assume !@\n";
        loop (count - 1)
      | Some _model as model -> run_model model (count - 1)
    end
    | Error (Trap trap) -> Some (`Trap trap, thread)
    | Error Assert_fail -> Some (`Assert_fail, thread)
  in
  loop 10

(* NB: This function propagates potential errors (Result.err) occurring
   during evaluation (OS, syntax error, etc.), except for Trap and Assert,
   which are handled here. Most of the computations are done in the Result
   monad, hence the let*. *)
let cmd profiling debug unsafe optimize workers no_stop_at_failure no_values
  deterministic_result_order (workspace : Fpath.t) solver profile files =
  begin
    match profile with None -> () | Some f -> Stats.init_logger_to_file f
  end;
  ignore (workers, no_stop_at_failure, deterministic_result_order, workspace);

  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;

  (* deterministic_result_order implies no_stop_at_failure *)
  (* let no_stop_at_failure = deterministic_result_order || no_stop_at_failure in *)
  let* _created_dir = Bos.OS.Dir.create ~path:true ~mode:0o755 workspace in
  let solver = Solver.fresh solver () in
  let* link_state, modules_to_run =
    simplify_then_link_files ~unsafe ~optimize files
  in
  let tree = fresh_tree [] in
  let result = launch solver tree link_state modules_to_run in

  let print_pc pc =
    Format.pp_std "PC:@\n";
    Format.pp_std "%a@\n" Concolic_choice.pp_pc pc
  in
  let print_values symbols_value =
    Format.pp_std "Assignments:@\n";
    List.iter
      (fun (s, v) ->
        Format.pp_std "  %a: %a" Smtml.Symbol.pp s Concrete_value.pp v )
      symbols_value;
    Format.pp_std "@\n"
  in

  let testcase model =
    if not no_values then
      let testcase =
        List.sort compare (Smtml.Model.get_bindings model) |> List.map snd
      in
      Cmd_utils.write_testcase ~dir:workspace testcase
    else Ok ()
  in

  match result with
  | None ->
    Format.pp_std "OK@\n";
    Ok ()
  | Some (`Trap trap, thread) ->
    Format.pp_std "Trap: %s@\n" (Trap.to_string trap);
    if debug then begin
      print_pc thread.pc;
      print_values thread.symbols_value
    end;
    let symbols = None in
    let model = get_model ~symbols solver thread.pc in
    Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model;
    let* () = testcase model in
    Error (`Found_bug 1)
  | Some (`Assert_fail, thread) ->
    Format.pp_std "Assert failure@\n";
    if debug then begin
      print_pc thread.pc;
      print_values thread.symbols_value
    end;
    let symbols = None in
    let model = get_model ~symbols solver thread.pc in
    Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model;
    let* () = testcase model in
    Error (`Found_bug 1)

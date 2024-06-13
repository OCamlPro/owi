(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax
module Expr = Smtml.Expr
module Value = Concolic_value.V
module Choice = Concolic.P.Choice

(* let () = Random.self_init () *)
let () = Random.init 42

let debug = false

let symbolic_extern_module :
  Concolic.P.Extern_func.extern_func Link.extern_module =
  let symbol_i32 () : Value.int32 Choice.t =
    Choice.with_new_symbol (Ty_bitv 32) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Random.bits32 ()
          | Some (Num (I32 n)) -> n
          | _ -> assert false
        in
        (I32 n, Value.pair n (Expr.mk_symbol sym)) )
  in
  let symbol_i8 () : Value.int32 Choice.t =
    Choice.with_new_symbol (Ty_bitv 32) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Int32.logand 0xFFl (Random.bits32 ())
          | Some (Num (I32 n)) -> n
          | _ -> assert false
        in
        let sym_expr =
          Expr.make (Cvtop (Ty_bitv 32, Zero_extend 24, Expr.mk_symbol sym))
        in
        (I32 n, Value.pair n sym_expr) )
  in
  let symbol_i64 () : Value.int64 Choice.t =
    Choice.with_new_symbol (Ty_bitv 64) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Random.bits64 ()
          | Some (Num (I64 n)) -> n
          | _ -> assert false
        in
        (I64 n, Value.pair n (Expr.mk_symbol sym)) )
  in
  let symbol_f32 () : Value.float32 Choice.t =
    Choice.with_new_symbol (Ty_fp 32) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Random.bits32 ()
          | Some (Num (F32 n)) -> n
          | _ -> assert false
        in
        let n = Float32.of_bits n in
        (F32 n, Value.pair n (Expr.mk_symbol sym)) )
  in
  let symbol_f64 () : Value.float64 Choice.t =
    Choice.with_new_symbol (Ty_fp 64) (fun sym forced_value ->
        let n =
          match forced_value with
          | None -> Random.bits64 ()
          | Some (Num (F64 n)) -> n
          | _ -> assert false
        in
        let n = Float64.of_bits n in
        (F64 n, Value.pair n (Expr.mk_symbol sym)) )
  in
  let assume_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Concolic_choice.assume c
  in
  let assume_positive_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.ge i Value.I32.zero in
    Concolic_choice.assume c
  in
  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Concolic_choice.assertion c
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i8_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i8)
      )
    ; ( "i32_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
      )
    ; ( "i64_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
      )
    ; ( "f32_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
      )
    ; ( "f64_symbol"
      , Concolic.P.Extern_func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
      )
    ; ( "assume"
      , Concolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_i32) )
    ; ( "assume_positive_i32"
      , Concolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_positive_i32) )
    ; ( "assert"
      , Concolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { functions }

let summaries_extern_module :
  Concolic.P.Extern_func.extern_func Link.extern_module =
  let open Expr in
  let i32 (v : Value.int32) =
    (* TODO: select_i32 ? *)
    (* let+ v = Choice.select_i32 v in *)
    (* let n = v.c in *)
    (* let x = Choice.assume (Value.I32.eq v (Value.const_i32 n)) in *)
    match view v.symbolic with
    | Val (Num (I32 v)) -> v
    | _ ->
      Log.err {|alloc: cannot allocate base pointer "%a"|} Expr.pp v.symbolic
  in
  let ptr (v : Value.int32) =
    match view v.symbolic with
    | Ptr (b, _) -> b
    | _ ->
      Log.err {|free: cannot fetch pointer base of "%a"|} Expr.pp v.symbolic
  in
  let abort () : unit Choice.t = Choice.abort in
  let alloc (base : Value.int32) (_size : Value.int32) : Value.int32 Choice.t =
    let base : int32 = i32 base in
    Choice.return
      { Concolic_value.concrete = base
      ; symbolic = Expr.make (Ptr (base, Symbolic_value.const_i32 0l))
      }
    (* WHAT ???? *)
    (* Choice.with_thread (fun t : Value.int32 -> *)
    (*     let memories = t.shared.memories in *)
    (*     Symbolic_memory.iter *)
    (*       (fun tbl -> *)
    (*         Symbolic_memory.ITbl.iter *)
    (*           (fun _ (m : Symbolic_memory.t) -> *)
    (*             Symbolic_memory.replace_size m base size.s ) *)
    (*           tbl ) *)
    (*       memories; *)
    (*     { c = base; s = Expr.make (Ptr (base, Symbolic_value.const_i32 0l)) }) *)
  in
  let free (p : Value.int32) : unit Choice.t =
    (* WHAT ???? *)
    let _base = ptr p in
    (* Choice.with_thread (fun t -> *)
    (*     let memories = t.shared.memories in *)
    (*     Symbolic_memory.iter *)
    (*       (fun tbl -> *)
    (*         Symbolic_memory.ITbl.iter *)
    (*           (fun _ (m : Symbolic_memory.t) -> Symbolic_memory.free m base) *)
    (*           tbl ) *)
    (*       memories ) *)
    Choice.return ()
  in
  let functions =
    [ ( "alloc"
      , Concolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) )
    ; ( "dealloc"
      , Concolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), free) )
    ; ("abort", Concolic.P.Extern_func.Extern_func (Func (UArg Res, R0), abort))
    ]
  in
  { functions }

let ( let** ) (t : 'a Result.t Choice.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  Choice.bind t (fun t ->
      match t with Error e -> Choice.return (Error e) | Ok x -> f x )

let simplify_then_link_text_module ~unsafe ~optimize link_state (m : Text.modul)
    =
  let has_start =
    List.exists (function Text.MStart _ -> true | _ -> false) m.fields
  in
  let has_start_id_function =
    List.exists
      (function Text.MFunc { id = Some "_start"; _ } -> true | _ -> false)
      m.fields
  in
  let fields =
    if has_start || not has_start_id_function then m.fields
    else MStart (Text "_start") :: m.fields
  in
  let m = { m with fields } in
  Compile.Text.until_link ~unsafe link_state ~optimize ~name:None m

let simplify_then_link_binary_module ~unsafe ~optimize link_state
  (m : Binary.modul) =
  let start =
    if Option.is_some m.start then m.start
    else
      match
        List.find_opt
          (function { Binary.name = "_start"; _ } -> true | _ -> false)
          m.exports.func
      with
      | None -> None
      | Some export -> Some export.id
  in
  let m = { m with start } in
  Compile.Binary.until_link ~unsafe link_state ~optimize ~name:None m

let simplify_then_link ~unsafe ~optimize link_state m =
  let+ m, link_state =
    match m with
    | Either.Left (Either.Left text_module) ->
      simplify_then_link_text_module ~unsafe ~optimize link_state text_module
    | Either.Left (Either.Right _text_script) ->
      Error (`Msg "can't run concolic interpreter on a script")
    | Either.Right binary_module ->
      let* binary_module = Cmd_utils.add_main_to_run binary_module in
      simplify_then_link_binary_module ~unsafe ~optimize link_state
        binary_module
  in
  let module_to_run = Concolic.convert_module_to_run m in
  (link_state, module_to_run)

let simplify_then_link_files ~unsafe ~optimize filenames =
  let link_state = Link.empty_state in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic"
      ~func_typ:Concolic.P.Extern_func.extern_type symbolic_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"summaries"
      ~func_typ:Concolic.P.Extern_func.extern_type summaries_extern_module
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
    Select { cond; if_true = fresh_tree pc; if_false = fresh_tree pc }
  | Select_i32 (value, _) -> Select_i32 { value; branches = IMap.empty }
  | Assume cond -> Assume { cond; cont = fresh_tree pc }
  | Assert cond -> Assert { cond; cont = fresh_tree pc; disproved = None }

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
    let pc = head_of_trace :: pc in
    try_initialize pc node head_of_trace;
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
  deterministic_result_order (workspace : Fpath.t) solver files =
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
      Testcase.write_testcase ~dir:workspace ~err:true testcase
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

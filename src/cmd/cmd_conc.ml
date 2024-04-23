open Syntax
module Expr = Smtml.Expr
module Value = Concolic_value.V
module Choice = Concolic.P.Choice

(* let () = Random.self_init () *)
let () = Random.init 42

let symbolic_extern_module :
  Concolic.P.Extern_func.extern_func Link.extern_module =
  let symbol ty () : Value.int32 Choice.t =
    Choice.with_new_symbol ty (fun sym forced_value ->
        let sym_expr = Expr.mk_symbol sym in
        let n =
          match forced_value with
          | None -> Random.bits32 ()
          | Some v -> (
            match v with
            | Int i -> Int32.of_int i
            | Num (I32 n) -> n
            | _ ->
              Format.pp_err "Wrong type of forced value %a@." Smtml.Value.pp v;
              Random.bits32 () )
        in
        let sym_expr =
          match ty with
          | Ty_bitv 8 -> Expr.make (Cvtop (Ty_bitv 32, Zero_extend 24, sym_expr))
          | _ -> sym_expr
        in
        (n, Value.pair n sym_expr) )
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
    [ (*   ( "i8_symbol" *)
      (*   , Symbolic.P.Extern_func.Extern_func *)
      (*       (Func (UArg Res, R1 I32), symbol (Ty_bitv 8)) ); *)
      ( "i32_symbol"
      , Concolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), symbol (Ty_bitv 32)) )
      (* ; ( "i64_symbol" *)
      (*   , Symbolic.P.Extern_func.Extern_func *)
      (*       (Func (UArg Res, R1 I64), symbol (Ty_bitv 64)) ) *)
      (* ; ( "f32_symbol" *)
      (*   , Symbolic.P.Extern_func.Extern_func *)
      (*       (Func (UArg Res, R1 F32), symbol (Ty_fp 32)) ) *)
      (* ; ( "f64_symbol" *)
      (*   , Symbolic.P.Extern_func.Extern_func *)
      (*       (Func (UArg Res, R1 F64), symbol (Ty_fp 64)) ) *)
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
  (* let open Expr in *)
  (* let i32 v = *)
  (*   match view v with *)
  (*   | Val (Num (I32 v)) -> v *)
  (*   | _ -> Log.err {|alloc: cannot allocate base pointer "%a"|} Expr.pp v *)
  (* in *)
  (* let ptr v = *)
  (*   match view v with *)
  (*   | Ptr (b, _) -> b *)
  (*   | _ -> Log.err {|free: cannot fetch pointer base of "%a"|} Expr.pp v *)
  (* in *)
  (* let abort () : unit Choice.t = Choice.add_pc @@ Value.Bool.const false in *)
  (* let alloc (base : Value.int32) (size : Value.int32) : Value.int32 Choice.t = *)
  (*   let base : int32 = i32 base in *)
  (*   Choice.with_thread (fun t -> *)
  (*       let memories = Thread.memories t in *)
  (*       Symbolic_memory.iter *)
  (*         (fun tbl -> *)
  (*           Symbolic_memory.ITbl.iter *)
  (*             (fun _ (m : Symbolic_memory.t) -> *)
  (*               Symbolic_memory.replace_size m base size ) *)
  (*             tbl ) *)
  (*         memories; *)
  (*       Expr.make (Ptr (base, Value.const_i32 0l)) ) *)
  (* in *)
  (* let free (p : Value.int32) : unit Choice.t = *)
  (*   let base = ptr p in *)
  (*   Choice.with_thread (fun t -> *)
  (*       let memories = Thread.memories t in *)
  (*       Symbolic_memory.iter *)
  (*         (fun tbl -> *)
  (*           Symbolic_memory.ITbl.iter *)
  (*             (fun _ (m : Symbolic_memory.t) -> Symbolic_memory.free m base) *)
  (*             tbl ) *)
  (*         memories ) *)
  (* in *)
  let functions =
    [ (*   ( "alloc" *)
      (*   , Symbolic.P.Extern_func.Extern_func *)
      (*       (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) ) *)
      (* ; ( "dealloc" *)
      (*   , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), free) ) *)
      (* ; ("abort", Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R0), abort)) *) ]
  in
  { functions }

let ( let** ) (t : 'a Result.t Choice.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  Choice.bind t (fun t ->
      match t with Error e -> Choice.return (Error e) | Ok x -> f x )

let simplify_then_link ~unsafe ~optimize link_state (m : Text.modul) =
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
  let+ m, link_state =
    Compile.until_link ~unsafe link_state ~optimize ~name:None m
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
        let* m0dule = Parse.Module.from_file filename in
        let+ link_state, module_to_run =
          simplify_then_link ~unsafe ~optimize link_state m0dule
        in
        (link_state, module_to_run :: modules_to_run) )
      (Ok (link_state, []))
      filenames
  in
  (link_state, List.rev modules_to_run)

let run_modules_to_run (link_state : _ Link.state) modules_to_run =
  let link_state_envs =
    (* TODO: fix (move globals/memories/tables back to thread and fix this)
       Or we can fix this by sharing the result of Concolic.convert_env
    *)
    Env_id.map Concolic.convert_env link_state.envs
  in
  List.fold_left
    (fun (acc : unit Result.t Concolic.P.Choice.t) to_run ->
      let** () = acc in
      (Interpret.Concolic.modul link_state_envs) to_run )
    (Choice.return (Ok ())) modules_to_run

let get_model (* ~symbols *) solver pc =
  let expr = Concolic_choice.pc_to_exprs pc in
  assert (`Sat = Solver.Z3Batch.check solver expr);
  match Solver.Z3Batch.model (* ~symbols *) solver with
  | None -> assert false
  | Some model -> model

type assignments = (Smtml.Symbol.t * Int32.t) list

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
  ; pc : Concolic_choice.pc
  ; mutable ends : (end_of_trace * assignments) list
  }

let fresh_tree pc = { node = Not_explored; pc; ends = [] }

let new_node pc (head : Concolic_choice.pc_elt) : node =
  match head with
  | Select (cond, _) ->
    Select { cond; if_true = fresh_tree pc; if_false = fresh_tree pc }
  | Select_i32 (value, _) -> Select_i32 { value; branches = IMap.empty }
  | Assume cond -> Assume { cond; cont = fresh_tree pc }
  | Assert cond -> Assert { cond; cont = fresh_tree pc; disproved = None }

let try_initialize pc node head =
  match node.node with Not_explored -> node.node <- new_node pc head | _ -> ()

let check = true

let rec add_trace pc node (trace : trace) =
  match trace.remaining_pc with
  | [] -> begin
    node.ends <- (trace.end_of_trace, trace.assignments) :: node.ends;
    match trace.end_of_trace with
    | Trap Unreachable -> begin
      match node.node with
      | Not_explored -> node.node <- Unreachable
      | Unreachable -> ()
      | _ -> assert false
    end
    | _ -> ()
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
      add_trace pc branch { trace with remaining_pc = tail_of_trace }
    | _, Select _ | Select _, _ -> assert false
    | Select_i32 { value; branches }, Select_i32 (value', v) ->
      if check then assert (Smtml.Expr.equal value value');
      let branch =
        match IMap.find_opt v branches with
        | None ->
          let t = fresh_tree pc in
          node.node <- Select_i32 { value; branches = IMap.add v t branches };
          t
        | Some t -> t
      in
      add_trace pc branch { trace with remaining_pc = tail_of_trace }
    | _, Select_i32 _ | Select_i32 _, _ -> assert false
    | Assume { cond; cont }, Assume cond' ->
      if check then assert (Smtml.Expr.equal cond cond');
      add_trace pc cont { trace with remaining_pc = tail_of_trace }
    | _, Assume _ | Assume _, _ -> assert false
    | Assert ({ cond; cont; disproved = _ } as assert_), Assert cond' ->
      if check then assert (Smtml.Expr.equal cond cond');
      begin
        match (tail_of_trace, trace.end_of_trace) with
        | [], Assert_fail -> assert_.disproved <- Some trace.assignments
        | _ -> ()
      end;
      add_trace pc cont { trace with remaining_pc = tail_of_trace } )

let add_trace tree trace = add_trace [] tree trace

let run_once tree link_state modules_to_run forced_values =
  let result = run_modules_to_run link_state modules_to_run in
  let ( ( result
        , Choice.{ pc; symbols = _; symbols_value; preallocated_values = _ } )
        as r ) =
    let forced_values =
      match forced_values with None -> Hashtbl.create 0 | Some v -> v
    in
    Choice.run forced_values result
  in
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
  let () =
    Format.pp_std "Add trace:@\n";
    Format.pp_std "%a@\n" Concolic_choice.pp_pc trace.remaining_pc
  in
  add_trace tree trace;
  r

(* Very naive ! *)
let rec find_node_to_run tree =
  match tree.node with
  | Not_explored ->
    Format.pp_std "Try unexplored@.%a@.@." Concolic_choice.pp_pc tree.pc;
    Some tree.pc
  | Select { cond = _; if_true; if_false } ->
    let b = Random.bool () in
    Format.pp_std "Select bool %b@." b;
    let tree = if b then if_true else if_false in
    find_node_to_run tree
  | Select_i32 { value = _; branches } ->
    (* TODO: better ! *)
    let branches = IMap.bindings branches in
    let n = List.length branches in
    if n = 0 then None
    else
      let i = Random.int n in
      let () = Format.pp_std "Select_i32 %i@." i in
      let _, branch = List.nth branches i in
      find_node_to_run branch
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
  let expr = Concolic_choice.pc_to_exprs pc in
  match Solver.Z3Batch.check solver expr with
  | `Unsat | `Unknown -> None
  | `Sat -> (
    match Solver.Z3Batch.model solver with
    | None -> assert false
    | Some model -> Some model )

let find_model_to_run solver tree =
  match find_node_to_run tree with
  | None -> None
  | Some pc -> pc_model solver pc

let launch solver tree link_state modules_to_run =
  let rec find_model n =
    if n = 0 then
      let () = Format.pp_std "Failed to find something to run@." in
      None
    else
      match find_model_to_run solver tree with
      | None -> find_model (n - 1)
      | Some m ->
        let () =
          Format.pp_std "Found something to run %a@."
            (fun ppf v -> Smtml.Model.pp ppf v)
            m
        in
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
      Format.pp_std "Assume_fail: %a@\n" Smtml.Expr.pp c;
      Format.pp_std "Assignments:@\n%a@\n" Concolic_choice.pp_assignments
        thread.symbols_value;
      Format.pp_std "Retry !@\n";
      match pc_model solver thread.pc with
      | None ->
        Format.pp_err "Can't satisfy assume !@\n";
        loop (count - 1)
      | Some model -> run_model (Some model) (count - 1)
    end
    | Error (Trap trap) -> Some (`Trap trap, thread)
    | Error Assert_fail -> Some (`Assert_fail, thread)
  in
  loop 10

(* TODO ICI: avec ce modèle, préparer un truc que symbol va pouvoir utiliser pour donner les valeurs de l'exécution *)

(* NB: This function propagates potential errors (Result.err) occurring
   during evaluation (OS, syntax error, etc.), except for Trap and Assert,
   which are handled here. Most of the computations are done in the Result
   monad, hence the let*. *)
let cmd profiling debug unsafe optimize workers no_stop_at_failure no_values
  deterministic_result_order (workspace : Fpath.t) files =
  ignore
    ( profiling
    , debug
    , unsafe
    , optimize
    , workers
    , no_stop_at_failure
    , no_values
    , deterministic_result_order
    , workspace
    , files );

  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;

  (* deterministic_result_order implies no_stop_at_failure *)
  (* let no_stop_at_failure = deterministic_result_order || no_stop_at_failure in *)
  (* let* () = *)
  (*   match Bos.OS.Dir.create ~path:true ~mode:0o755 workspace with *)
  (*   | Ok true | Ok false -> Ok () *)
  (*   | Error (`Msg msg) -> Error (`Msg msg) *)
  (* in *)
  let solver = Solver.Z3Batch.create () in
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
      (fun (s, v) -> Format.pp_std "  %a: %li" Smtml.Symbol.pp s v)
      symbols_value;
    Format.pp_std "@\n"
  in

  let () =
    match result with
    | None -> Format.pp_std "OK@\n"
    | Some (`Trap trap, thread) ->
      Format.pp_std "Trap: %s@\n" (Trap.to_string trap);
      print_pc thread.pc;
      print_values thread.symbols_value;
      let model = get_model solver thread.pc in
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
    | Some (`Assert_fail, thread) ->
      Format.pp_std "Assert failure@\n";
      print_pc thread.pc;
      print_values thread.symbols_value;
      let model = get_model solver thread.pc in
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
  in

  Ok ()

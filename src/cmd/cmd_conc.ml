open Syntax
module Expr = Smtml.Expr
module Value = Concolic_value.V
module Choice = Concolic.P.Choice

let () = Random.self_init ()

let symbolic_extern_module :
  Concolic.P.Extern_func.extern_func Link.extern_module =
  let symbol ty () : Value.int32 Choice.t =
    Choice.with_new_symbol ty (fun sym ->
        let sym_expr = Expr.mk_symbol sym in
        let n = Random.bits32 () in
        let sym_expr =
          match ty with
          | Ty_bitv 8 -> Expr.make (Cvtop (Ty_bitv 32, Zero_extend 24, sym_expr))
          | _ -> sym_expr
        in
        let sym_value = Smtml.Expr.Bitv.I32.v n in
        sym_value, Value.pair n sym_expr )
  in
  let assume_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Concolic_choice.assume c
  in
  (* let assume_positive_i32 (i : Value.int32) : unit Choice.t = *)
  (*   let c = Value.I32.ge i Value.I32.zero in *)
  (*   Choice.add_pc c *)
  (* in *)
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
      (* ; ( "assume_positive_i32" *)
      (*   , Symbolic.P.Extern_func.Extern_func *)
      (*       (Func (Arg (I32, Res), R0), assume_positive_i32) ) *)
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

let ( let*/ ) (t : 'a Result.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  match t with Error e -> Choice.return (Error e) | Ok x -> f x

let simplify_then_link_then_run ~unsafe ~optimize (pc : unit Result.t Choice.t)
  (m : Text.modul) =
  let link_state = Link.empty_state in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic"
      ~func_typ:Concolic.P.Extern_func.extern_type symbolic_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"summaries"
      ~func_typ:Concolic.P.Extern_func.extern_type summaries_extern_module
  in
  let*/ to_run, link_state =
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
    let+ m, state =
      Compile.until_link ~unsafe link_state ~optimize ~name:None m
    in
    let m = Concolic.convert_module_to_run m in
    (m, state)
  in
  let link_state_envs =
    (* TODO: fix (move globals/memories/tables back to thread and fix this)
       Or we can fix this by sharing the result of Concolic.convert_env
    *)
    Env_id.map Concolic.convert_env link_state.envs
  in
  let c = (Interpret.Concolic.modul link_state_envs) to_run in
  Choice.bind pc (fun r ->
      match r with Error _ -> Choice.return r | Ok () -> c )

let run_file ~unsafe ~optimize pc filename =
  let*/ m0dule = Parse.Module.from_file filename in
  simplify_then_link_then_run ~unsafe ~optimize pc m0dule

let get_model (* ~symbols *) solver pc =
  let expr = Concolic_choice.pc_to_exprs pc in
  assert (`Sat = Solver.Z3Batch.check solver expr);
  match Solver.Z3Batch.model (* ~symbols *) solver with
  | None -> assert false
  | Some model -> model

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
  let result =
    List.fold_left (run_file ~unsafe ~optimize) (Choice.return (Ok ())) files
  in
  let result, Choice.{ pc; symbols; symbols_value } = Choice.run result in

  let print_pc () =
    Format.pp_std "PC:@\n";
    Format.pp_std "%a@\n" Concolic_choice.pp_pc pc
  in
  let print_values () =
    Format.pp_std "Assignments:@\n";
    List.iter (fun (s, v) -> Format.pp_std "  %a: %a" Smtml.Symbol.pp s Expr.pp v) symbols_value;
    Format.pp_std "@\n"
  in

  let () =
    match result with
    | Ok (Ok ()) -> begin
      Format.pp_std "OK@\n";
      Format.pp_std "Symbols: %i@\n" symbols;
      print_pc ();
      print_values ();
      let ok_model = get_model (* ~symbols TODO ? *) solver pc in
      Format.pp_std "Model:@\n  @[<v>%a@]@."
        (Smtml.Model.pp ~no_values)
        ok_model
      (* TODO rerun with something else *)
    end
    | Ok (Error e) -> Result.failwith e
    | Error (Assume_fail c) ->
      Format.pp_std "Assume_fail: %a@\n" Smtml.Expr.pp c;
      print_pc ();
      print_values ();
      let model = get_model solver pc in
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
    | Error (Trap trap) ->
      Format.pp_std "Trap: %s@\n" (Trap.to_string trap);
      print_pc ();
      print_values ();
      let model = get_model solver pc in
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
    | Error Assert_fail ->
      Format.pp_std "Assert failure@\n";
      print_pc ();
      print_values ();
      let model = get_model solver pc in
      Format.pp_std "Model:@\n  @[<v>%a@]@." (Smtml.Model.pp ~no_values) model
  in

  Ok ()

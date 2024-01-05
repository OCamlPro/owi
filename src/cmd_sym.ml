open Syntax
module Expr = Encoding.Expr
module Value = Symbolic_value.S
module Choice = Symbolic.P.Choice
module Solver = Thread.Solver

(* TODO: make this a CLI flag *)
let print_solver_time = false

let print_path_condition = false

let print_extern_module : Symbolic.P.extern_func Link.extern_module =
  let print_i32 (i : Value.int32) : unit Choice.t =
    Format.pp_std "%s@\n" (Expr.to_string i);
    Choice.return ()
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i32"
      , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), print_i32)
      )
    ]
  in
  { functions }

let assert_extern_module : Symbolic.P.extern_func Link.extern_module =
  let positive_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.ge i Value.I32.zero in
    Choice.add_pc c
  in
  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.add_pc c
  in
  (* we need to describe their types *)
  let functions =
    [ ( "positive_i32"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), positive_i32) )
    ; ( "i32"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { functions }

let symbolic_extern_module : Symbolic.P.extern_func Link.extern_module =
  let sym_cnt = Atomic.make 0 in
  let mk_symbol = Encoding.Symbol.mk_symbol in
  let symbol ty () : Value.int32 Choice.t =
    let id = Atomic.fetch_and_add sym_cnt 1 in
    let r = Expr.mk_symbol @@ mk_symbol ty (Format.sprintf "symbol_%i" id) in
    match ty with
    | Ty_bitv S8 -> Choice.return @@ Expr.(Cvtop (ExtU 24, r) @: Ty_bitv S32)
    | _ -> Choice.return r
  in
  let assume_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.add_pc c
  in
  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.assertion c
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i8_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), symbol (Ty_bitv S8)) )
    ; ( "i32_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), symbol (Ty_bitv S32)) )
    ; ( "i64_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I64), symbol (Ty_bitv S64)) )
    ; ( "f32_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 F32), symbol (Ty_fp S32)) )
    ; ( "f64_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 F64), symbol (Ty_fp S64)) )
    ; ( "assume"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_i32) )
    ; ( "assert"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { functions }

let summaries_extern_module : Symbolic.P.extern_func Link.extern_module =
  let open Expr in
  let i32 v =
    match v.e with
    | Val (Num (I32 v)) -> v
    | _ -> Log.err {|alloc: cannot allocate base pointer "%a"|} Expr.pp v
  in
  let ptr v =
    match v.e with
    | Ptr (b, _) -> b
    | _ -> Log.err {|free: cannot fetch pointer base of "%a"|} Expr.pp v
  in
  let abort () : unit Choice.t = Choice.add_pc @@ Value.Bool.const false in
  let alloc (base : Value.int32) (size : Value.int32) : Value.int32 Choice.t =
    let base : int32 = i32 base in
    Choice.with_thread (fun t ->
        let memories = Thread.memories t in
        Env_id.Tbl.iter
          (fun _ tbl ->
            Symbolic_memory.ITbl.iter
              (fun _ (m : Symbolic_memory.M.t) ->
                Hashtbl.replace m.chunks base size )
              tbl )
          memories;
        Ptr (base, Value.const_i32 0l) @: Ty_bitv S32 )
  in
  let free (p : Value.int32) : unit Choice.t =
    let base = ptr p in
    Choice.with_thread (fun t ->
        let memories = Thread.memories t in
        Env_id.Tbl.iter
          (fun _ tbl ->
            Symbolic_memory.ITbl.iter
              (fun _ (m : Symbolic_memory.M.t) ->
                if not (Hashtbl.mem m.chunks base) then
                  (* TODO: trap instead? *)
                  failwith "Memory leak double free";
                Hashtbl.remove m.chunks base )
              tbl )
          memories )
  in
  let functions =
    [ ( "alloc"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) )
    ; ( "dealloc"
      , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), free) )
    ; ("abort", Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R0), abort))
    ]
  in
  { functions }

let ( let*/ ) (t : 'a Result.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  match t with Error e -> Choice.return (Error e) | Ok x -> f x

let simplify_then_link_then_run ~unsafe ~optimize (pc : unit Result.t Choice.t)
  file =
  let link_state = Link.empty_state in
  let link_state =
    Link.extern_module' link_state ~name:"print"
      ~func_typ:Symbolic.P.Extern_func.extern_type print_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"assert"
      ~func_typ:Symbolic.P.Extern_func.extern_type assert_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic"
      ~func_typ:Symbolic.P.Extern_func.extern_type symbolic_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"summaries"
      ~func_typ:Symbolic.P.Extern_func.extern_type summaries_extern_module
  in
  let*/ to_run, link_state =
    list_fold_left
      (fun ((to_run, state) as acc) instruction ->
        match instruction with
        | Text.Module m ->
          let has_start =
            List.exists (function Text.MStart _ -> true | _ -> false) m.fields
          in
          let has_start_id_function =
            List.exists
              (function
                | Text.MFunc { id = Some "_start"; _ } -> true | _ -> false )
              m.fields
          in
          let fields =
            if has_start || not has_start_id_function then m.fields
            else MStart (Text "_start") :: m.fields
          in
          let m = { m with fields } in
          let* m, state =
            Compile.until_link ~unsafe state ~optimize ~name:None m
          in
          let m = Symbolic.convert_module_to_run m in
          Ok (m :: to_run, state)
        | Text.Register (name, id) ->
          let* state = Link.register_module state ~name ~id in
          Ok (to_run, state)
        | _ -> Ok acc )
      ([], link_state) file
  in
  let f (pc : (unit, _) result Choice.t) to_run =
    let c = (Interpret.Symbolic.modul link_state.envs) to_run in
    let results =
      Choice.bind pc (fun r ->
          match r with Error _ -> Choice.return r | Ok () -> c )
    in
    results
  in
  List.fold_left f pc (List.rev to_run)

let run_file ~unsafe ~optimize (pc : unit Result.t Choice.t) filename =
  if not @@ Sys.file_exists filename then
    Choice.return (error_s "file `%s` doesn't exist" filename)
  else
    let*/ script = Parse.Script.from_file ~filename in
    simplify_then_link_then_run ~unsafe ~optimize pc script

let get_model solver pc =
  assert (Solver.check solver pc);
  match Solver.model solver with None -> assert false | Some model -> model

let mkdir_p_exn dir =
  let rec get_intermediate_dirs d acc =
    if Sys.file_exists d then acc
    else get_intermediate_dirs (Filename.dirname d) (d :: acc)
  in
  let intermediate_dirs = get_intermediate_dirs dir [] in
  List.iter (fun d -> Sys.mkdir d 0o755) intermediate_dirs

let out_testcase ~dst ~err testcase =
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) dst in
  let tag ?(atts = []) name = (("", name), atts) in
  let atts = if err then Some [ (("", "coversError"), "true") ] else None in
  let to_string v = Format.asprintf "%a" Encoding.Value.pp_num v in
  let input v = `El (tag "input", [ `Data (to_string v) ]) in
  let testcase = `El (tag ?atts "testcase", List.map input testcase) in
  let dtd =
    {|<!DOCTYPE testcase PUBLIC "+//IDN sosy-lab.org//DTD test-format testcase 1.1//EN" "https://sosy-lab.org/test-format/testcase-1.1.dtd">|}
  in
  Xmlm.output o (`Dtd (Some dtd));
  Xmlm.output_tree Fun.id o testcase

let write_testcase =
  let cnt = ref 0 in
  fun ~dir ~err testcase ->
    incr cnt;
    let name = Format.sprintf "testcase-%d.xml" !cnt in
    let path = Filename.concat dir name in
    let out_chan = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out out_chan)
      (fun () -> out_testcase ~dst:(`Channel out_chan) ~err testcase)

let cmd profiling debug unsafe optimize workers no_stop_at_failure workspace
  files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  mkdir_p_exn workspace;
  let pc = Choice.return (Ok ()) in
  let solver = Solver.create () in
  let result = List.fold_left (run_file ~unsafe ~optimize) pc files in
  let thread : Thread.t = Thread.create () in
  let results = Choice.run_and_trap ~workers result thread in
  let failing =
    Seq.filter_map
      (fun (result, thread) ->
        let pc = Thread.pc thread in
        if print_path_condition then
          Format.pp_std "PATH CONDITION:@\n%a@\n" Expr.pp_list pc;
        let model = get_model solver pc in
        let result =
          match result with
          | Choice_intf.EVal (Ok ()) -> None
          | EAssert assertion ->
            Format.pp_std "Assert failure: %a@\n" Expr.pp assertion;
            Format.pp_std "Model:@\n  @[<v>%a@]@\n" Encoding.Model.pp model;
            Some pc
          | ETrap tr ->
            Format.pp_std "Trap: %s@\n" (Trap.to_string tr);
            Format.pp_std "Model:@\n  @[<v>%a@]@\n" Encoding.Model.pp model;
            Some pc
          | EVal (Error e) ->
            Format.pp_err "Error: %s@\n" e;
            exit 1
        in
        let testcase =
          List.sort
            (fun (x1, _) (x2, _) -> compare x1 x2)
            (Encoding.Model.get_bindings model)
          |> List.map snd
        in
        write_testcase ~dir:workspace ~err:(Option.is_some result) testcase;
        result )
      results
  in
  let () =
    if no_stop_at_failure then
      let failures = Seq.fold_left (fun n _ -> succ n) 0 failing in
      if failures = 0 then Format.pp_std "All OK@\n"
      else Format.pp_err "Reached %i problems!@\n" failures
    else
      match failing () with
      | Nil -> Format.pp_std "All OK@\n"
      | Cons (_thread, _) -> Format.pp_err "Reached problem!@\n"
  in
  let time = !Thread.Solver.solver_time in
  let count = !Thread.Solver.solver_count in
  if print_solver_time then begin
    Format.pp_std "@\n";
    Format.pp_std "Solver time %fs@\n" time;
    Format.pp_std "      calls %i@\n" count;
    Format.pp_std "  mean time %fms@\n" (1000. *. time /. float count)
  end

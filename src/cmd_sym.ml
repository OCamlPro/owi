open Syntax
module Expr = Encoding.Expression
module Value = Symbolic_value.S
module Choice = Symbolic.P.Choice
module Solver = Thread.Solver

let print_extern_module : Symbolic.P.extern_func Link.extern_module =
  let print_i32 (i : Value.int32) : unit Choice.t =
    Printf.printf "%s\n%!" (Encoding.Expression.to_string i);
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

let names = [| "plop"; "foo"; "bar" |]

let symbolic_extern_module : Symbolic.P.extern_func Link.extern_module =
  let counter = ref 0 in
  let symbolic_i32 (i : Value.int32) : Value.int32 Choice.t =
    let name =
      match i with
      | Encoding.Expression.Val (Num (I32 i)) -> begin
        match names.(Int32.to_int i) with exception _ -> "x" | name -> name
      end
      | _ ->
        failwith
          (Printf.sprintf "Text name %s" (Encoding.Expression.to_string i))
    in
    incr counter;
    let r =
      Encoding.Expression.mk_symbol_s `I32Type
        (Printf.sprintf "%s_%i" name !counter)
    in
    Choice.return r
  in
  let symbol_i32 () : Value.int32 Choice.t =
    incr counter;
    let r =
      Encoding.Expression.mk_symbol_s `I32Type
        (Printf.sprintf "symbol_%i" !counter)
    in
    Choice.return r
  in
  let symbol_i64 () : Value.int64 Choice.t =
    incr counter;
    let r =
      Encoding.Expression.mk_symbol_s `I64Type
        (Printf.sprintf "symbol_%i" !counter)
    in
    Choice.return r
  in
  let symbol_f32 () : Value.float32 Choice.t =
    incr counter;
    let r =
      Encoding.Expression.mk_symbol_s `F32Type
        (Printf.sprintf "symbol_%i" !counter)
    in
    Choice.return r
  in
  let symbol_f64 () : Value.float64 Choice.t =
    incr counter;
    let r =
      Encoding.Expression.mk_symbol_s `F64Type
        (Printf.sprintf "symbol_%i" !counter)
    in
    Choice.return r
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
    [ ( "i32"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), symbolic_i32) )
    ; ( "i32_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
      )
    ; ( "i64_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 I64), symbol_i64)
      )
    ; ( "f32_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 F32), symbol_f32)
      )
    ; ( "f64_symbol"
      , Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R1 F64), symbol_f64)
      )
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
  let alloc (base : Value.int32) (_size : Value.int32) : Value.int32 Choice.t =
    Choice.return base
  in
  let dealloc (_base : Value.int32) : unit Choice.t = Choice.return () in
  let is_symbolic (_a : Value.int32) (_n : Value.int32) : Value.int32 Choice.t =
    (* TODO: load n bytes from address a and check if it's a Val *)
    Choice.return @@ Value.const_i32 0l
  in
  let functions =
    [ ( "alloc"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) )
    ; ( "dealloc"
      , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), dealloc)
      )
    ; ( "is_symbolic"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), is_symbolic) )
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

let get_model thread =
  let (S (solver_mod, solver)) = Thread.solver thread in
  let module Solver = (val solver_mod) in
  assert (Solver.check solver (Thread.pc thread));
  match Solver.model solver with
  | None -> assert false
  | Some model -> model

let stop_at_first_failure = true

let cmd profiling debug unsafe optimize workers files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let pc = Choice.return (Ok ()) in
  let result = List.fold_left (run_file ~unsafe ~optimize) pc files in
  let thread : Thread.t = Thread.create () in
  let results = Choice.run_and_trap ~workers result thread in
  let failing =
    Seq.filter_map
      (fun (result, thread) ->
        let pc = Thread.pc thread in
        Format.printf "PATH CONDITION:@.";
        Format.printf "%a@." Expr.pp_list pc;
        match result with
        | Choice_monad_intf.EVal (Ok ()) -> None
        | EAssert assertion ->
          Format.printf "Assert failure: %a@." Expr.pp assertion;
          let model = get_model thread in
          Format.printf "Model:@.%a@." Encoding.Model.pp model;
          Some pc
        | ETrap tr ->
          Format.printf "TRAP: %s@." (Trap.to_string tr);
          let model = get_model thread in
          Format.printf "Model:@.%a@." Encoding.Model.pp model;
          Some pc
        | EVal (Error e) ->
          Format.eprintf "%s@." e;
          exit 1 )
      results
  in
  let () =
    if stop_at_first_failure then
      match failing () with
      | Nil -> Format.printf "All OK@."
      | Cons (_thread, _) -> Format.printf "Reached problem!@."
    else
      let failures = Seq.fold_left (fun n _ -> succ n) 0 failing in
      if failures = 0 then Format.printf "All OK@."
      else Format.printf "Reached %i problems!@." failures
  in
  let time = !Thread.Solver.solver_time in
  let count = !Thread.Solver.solver_count in
  Format.printf "@.";
  Format.printf "Solver time %fs@." time;
  Format.printf "      calls %i@." count;
  Format.printf "  mean time %fms@." (1000. *. time /. float count);
  ()

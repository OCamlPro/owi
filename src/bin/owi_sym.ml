open Owi
open Syntax
module Value = Sym_value.S
module Choice = Sym_state.P.Choice
module Solver = Thread.Solver

let print_extern_module : Sym_state.P.extern_func Link.extern_module =
  let print_i32 (i : Value.int32) : unit Choice.t =
    Printf.printf "%s\n%!" (Encoding.Expression.to_string i);
    Choice.return ()
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i32"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), print_i32) )
    ]
  in
  { functions }

let assert_extern_module : Sym_state.P.extern_func Link.extern_module =
  let positive_i32 (i : Value.int32) : unit Choice.t =
    let c = Sym_value.S.I32.ge i Sym_value.S.I32.zero in
    Choice.add_pc c
  in
  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Sym_value.S.I32.to_bool i in
    Choice.add_pc c
  in
  (* we need to describe their types *)
  let functions =
    [ ( "positive_i32"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), positive_i32) )
    ; ( "i32"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { functions }

let names = [| "plop"; "foo"; "bar" |]

let symbolic_extern_module : Sym_state.P.extern_func Link.extern_module =
  let counter = ref 0 in
  let symbolic_i32 (i : Value.int32) : Sym_value.S.int32 Choice.t =
    let name =
      match i with
      | Encoding.Expression.Val (Num (I32 i)) -> begin
        match names.(Int32.to_int i) with exception _ -> "x" | name -> name
      end
      | _ ->
        failwith
          (Printf.sprintf "Symbolic name %s" (Encoding.Expression.to_string i))
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
  let assume_i32 (i : Value.int32) : unit Choice.t =
    let c = Sym_value.S.I32.to_bool i in
    Choice.add_pc c
  in
  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Sym_value.S.I32.to_bool i in
    Choice.assertion c
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i32"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), symbolic_i32) )
    ; ( "i32_symbol"
      , Sym_state.P.Extern_func.Extern_func (Func (UArg Res, R1 I32), symbol_i32)
      )
    ; ( "assume"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_i32) )
    ; ( "assert"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { functions }

let summaries_extern_module : Sym_state.P.extern_func Link.extern_module =
  let alloc (base : Value.int32) (_size : Value.int32) : Value.int32 Choice.t =
    Choice.return base
  in
  let dealloc (_base : Value.int32) : unit Choice.t = Choice.return () in
  let functions =
    [ ( "alloc"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) )
    ; ( "dealloc"
      , Sym_state.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), dealloc)
      )
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
      ~func_typ:Sym_state.P.Extern_func.extern_type print_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"assert"
      ~func_typ:Sym_state.P.Extern_func.extern_type assert_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic"
      ~func_typ:Sym_state.P.Extern_func.extern_type symbolic_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"summaries"
      ~func_typ:Sym_state.P.Extern_func.extern_type summaries_extern_module
  in
  let*/ to_run, link_state =
    list_fold_left
      (fun ((to_run, state) as acc) instruction ->
        match instruction with
        | Symbolic.Module m ->
          let has_start =
            List.exists
              (function Symbolic.MStart _ -> true | _ -> false)
              m.fields
          in
          let has_start_id_function =
            List.exists
              (function
                | Symbolic.MFunc { id = Some "_start"; _ } -> true | _ -> false
                )
              m.fields
          in
          let fields =
            if has_start || not has_start_id_function then m.fields
            else MStart (Symbolic "_start") :: m.fields
          in
          let m = { m with fields } in
          let* m, state =
            Compile.until_link ~unsafe state ~optimize ~name:None m
          in
          let m = Sym_state.convert_module_to_run m in
          Ok (m :: to_run, state)
        | Symbolic.Register (name, id) ->
          let* state = Link.register_module state ~name ~id in
          Ok (to_run, state)
        | _ -> Ok acc )
      ([], link_state) file
  in
  let f (pc : (unit, _) result Choice.t) to_run =
    let c = (Interpret.S.modul link_state.envs) to_run in
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

(* Command line *)

let files =
  let doc = "source files" in
  let parse s = Ok s in
  Cmdliner.Arg.(
    value
    & pos 0
        (list ~sep:' ' (conv (parse, Format.pp_print_string)))
        [] (info [] ~doc) )

let debug =
  let doc = "debug mode" in
  Cmdliner.Arg.(value & flag & info [ "debug"; "d" ] ~doc)

let optimize =
  let doc = "optimize mode" in
  Cmdliner.Arg.(value & flag & info [ "optimize" ] ~doc)

let profiling =
  let doc = "profiling mode" in
  Cmdliner.Arg.(value & flag & info [ "profiling"; "p" ] ~doc)

let script =
  let doc = "run as a reference test suite script" in
  Cmdliner.Arg.(value & flag & info [ "script"; "s" ] ~doc)

let unsafe =
  let doc = "skip typechecking pass" in
  Cmdliner.Arg.(value & flag & info [ "unsafe"; "u" ] ~doc)

let get_model (solver : Thread.Solver.t) thread =
  assert (Thread.Solver.check solver (Thread.pc thread));
  match Thread.Solver.model solver with
  | None -> assert false
  | Some model -> Encoding.Model.to_string model

let stop_at_first_failure = true

let main profiling debug _script unsafe optimize files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let solver = Thread.Solver.create () in
  let pc = Choice.return (Ok ()) in
  let result = List.fold_left (run_file ~unsafe ~optimize) pc files in
  let thread : Thread.t = Thread.create () in
  let results = Choice.run_and_trap result thread in
  let failing =
    Seq.filter_map
      (fun (result, thread) ->
        Format.printf "PATH CONDITION:@.";
        List.iter
          (fun c -> print_endline (Encoding.Expression.to_string c))
          (Thread.pc thread);
        match result with
        | Choice_monad_intf.EVal (Ok ()) -> None
        | EAssert assertion ->
          Format.printf "Assert failure: %s@." assertion;
          let model = get_model solver thread in
          Format.printf "Model:@.%s@." model;
          Some thread
        | ETrap tr ->
          Format.printf "TRAP: %s@." (Trap.to_string tr);
          let model = get_model solver thread in
          Format.printf "Model:@.%s@." model;
          Some thread
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

let cli =
  let open Cmdliner in
  let doc = "OCaml WebAssembly Interpreter" in
  let man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ] in
  let info = Cmd.info "owi" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info
    Term.(const main $ profiling $ debug $ script $ unsafe $ optimize $ files)

let main () = exit @@ Cmdliner.Cmd.eval cli

let () = main ()

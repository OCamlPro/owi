open Owi
open Syntax

let print_extern_module : Sym_state.P.extern_func Link.extern_module =
  let print_i32 (i : Sym_value.S.int32) =
    Printf.printf "%s\n%!" (Encoding.Expression.to_string i)
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i32"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), print_i32) )
    ]
  in
  { functions }

let names = [| "plop"; "foo"; "bar" |]

let symbolic_extern_module : Sym_state.P.extern_func Link.extern_module =
  let counter = ref 0 in
  let symbolic_i32 (i : Sym_value.S.int32) : Sym_value.S.int32 =
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
    Encoding.Expression.mk_symbol_s `I32Type
      (Printf.sprintf "%s_%i" name !counter)
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i32"
      , Sym_state.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R1 I32), symbolic_i32) )
    ]
  in
  { functions }

let simplify_then_link_then_run ~optimize pc file =
  let link_state = Link.empty_state in
  let link_state =
    Link.extern_module' link_state ~name:"print"
      ~func_typ:Sym_state.P.Extern_func.extern_type print_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic"
      ~func_typ:Sym_state.P.Extern_func.extern_type symbolic_extern_module
  in
  let* to_run, link_state =
    list_fold_left
      (fun ((to_run, state) as acc) instruction ->
        match instruction with
        | Symbolic.Module m ->
          let* m, state = Compile.until_link state ~optimize ~name:None m in
          let m = Sym_state.convert_module_to_run m in
          Ok (m :: to_run, state)
        | Symbolic.Register (name, id) ->
          let* state = Link.register_module state ~name ~id in
          Ok (to_run, state)
        | _ -> Ok acc )
      ([], link_state) file
  in
  let f pc to_run =
    let c = (Interpret2.S.modul link_state.envs) to_run in
    let results = List.flatten @@ List.map c pc in
    let results =
      List.map (function Ok (), t -> t | Error _, _ -> assert false) results
    in
    Ok results
  in
  list_fold_left f [ pc ] (List.rev to_run)

let run_file ~optimize (pc : _ list) filename =
  if not @@ Sys.file_exists filename then
    error_s "file `%s` doesn't exist" filename
  else
    let* script = Parse.Script.from_file ~filename in
    Ok
      ( List.flatten
      @@ List.map
           (fun pc ->
             match simplify_then_link_then_run ~optimize pc script with
             | Ok v -> v
             | Error _ -> [] )
           pc )

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

let main profiling debug _script optimize files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let solver = Encoding.Batch.create () in
  let pc = [ Sym_state.P.{ solver; pc = []; mem = Sym_memory.memory } ] in
  let result = list_fold_left (run_file ~optimize) pc files in
  match result with
  | Ok results ->
    List.iter
      (fun thread ->
        Format.printf "PATH CONDITION:@.";
        List.iter
          (fun c -> print_endline (Encoding.Expression.to_string c))
          thread.Sym_state.P.pc )
      results
  | Error e ->
    Format.eprintf "%s@." e;
    exit 1

let cli =
  let open Cmdliner in
  let doc = "OCaml WebAssembly Interpreter" in
  let man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ] in
  let info = Cmd.info "owi" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info Term.(const main $ profiling $ debug $ script $ optimize $ files)

let main () = exit @@ Cmdliner.Cmd.eval cli

let () = main ()

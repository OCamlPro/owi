let error msg =
  Format.eprintf "error: %s@." msg;
  exit 1

let extern_module : Woi.Link_bis.extern_module =
  let open Woi in
  let module M = struct
    let rint : int32 ref Value.Extern_ref.ty = Value.Extern_ref.fresh "int ref"

    let fresh i = ref i

    let set r (i : int32) = r := i

    let get r : int32 = !r
  end in
  let print_i32 (i : Int32.t) = Printf.printf "%li\n%!" i in
  let functions =
    [ ( "print_i32"
      , Value.Func.Extern_func (Func (Arg (I32, Res), R0), print_i32) )
    ; ( "fresh"
      , Value.Func.Extern_func
          (Func (Arg (I32, Res), R1 (Externref M.rint)), M.fresh) )
    ; ( "set_i32r"
      , Value.Func.Extern_func
          (Func (Arg (Externref M.rint, Arg (I32, Res)), R0), M.set) )
    ; ( "get_i32r"
      , Value.Func.Extern_func
          (Func (Arg (Externref M.rint, Res), R1 I32), M.get) )
    ]
  in
  { functions }

let simplify_then_link_then_run file =
  let cmds =
    List.filter_map
      (function
        | Woi.Types.Module m -> Some (`Module (Woi.Simplify_bis.simplify m))
        | Woi.Types.Register (name, id) -> Some (`Register (name, id))
        | _ -> None )
      file
  in
  let () = Woi.Debug.debugerr "* Simplified %i modules@." (List.length cmds) in
  let link_state = Woi.Link_bis.empty_state in
  let link_state =
    Woi.Link_bis.link_extern_module "stuff" extern_module link_state
  in
  let to_run, _link_state =
    List.fold_left
      (fun (to_run, state) cmd ->
        match cmd with
        | `Module module_ ->
          let module_to_run, state = Woi.Link_bis.link_module module_ state in
          (module_to_run :: to_run, state)
        | `Register (name, id) ->
          (to_run, Woi.Link_bis.register_module state ~name ~id) )
      ([], link_state) cmds
  in
  let () = Woi.Debug.debugerr "* Linked@." in
  List.iter Woi.Interpret_bis.exec_module (List.rev to_run);
  let () = Woi.Debug.debugerr "* Done@." in
  ()

let (run_as_script, debug, files) =
  let run_as_script = ref false in
  let debug = ref false in
  let files = ref [] in
  let spec = Arg.[
      "--script", Set run_as_script, "run as a reference test suite script";
      "-s", Set run_as_script, "short for --script";
      "--debug", Set debug, "debug mode";
      "-d", Set debug, "short for --debug";
    ]
  in
  Arg.parse spec (fun s -> files := s :: !files) "wast interpreter %s <file>";
  !run_as_script, !debug, !files


let () =
  if debug then Woi.Debug.enable ();

  List.iter (fun file ->
      if not @@ Sys.file_exists file then
        error (Format.sprintf "file `%s` doesn't exist" file);

      match Woi.Parse.from_file ~filename:file with
      | Ok script -> begin
        Format.printf "%a@." Woi.Pp.Input.file script;
        if run_as_script then
          Woi.Script_bis.exec script
        else
          simplify_then_link_then_run script
      end
      | Error e -> error e)
    files

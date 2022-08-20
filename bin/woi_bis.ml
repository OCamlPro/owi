let error msg =
  Format.eprintf "error: %s@." msg;
  exit 1

let () =
  let argc = Array.length Sys.argv in

  if argc < 2 then
    error (Format.sprintf "usage: %s <file> [--debug]" Sys.argv.(0));

  let file = Sys.argv.(1) in

  if not @@ Sys.file_exists file then
    error (Format.sprintf "file `%s` doesn't exist" file);

  let debug = argc = 3 && Sys.argv.(2) = "--debug" in
  if debug then Woi.Debug.enable ();

  match Woi.Parse.from_file ~filename:file with
  | Ok script -> begin
    Format.printf "%a@." Woi.Pp.Input.file script;
    let cmds =
      List.filter_map
        (function
          | Woi.Types.Module m -> Some (`Module (Woi.Simplify_bis.simplify m))
          | Woi.Types.Register (name, id) -> Some (`Register (name, id))
          | _ -> None )
        script
    in
    let assertions =
      List.filter_map
        (function Woi.Types.Assert a -> Some a | _ -> None)
        script
    in
    let () = Format.printf "simplified %i@." (List.length cmds) in
    let link_state = Woi.Link_bis.empty_state in
    let to_run, link_state =
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
    List.iter Woi.Interpret_bis.exec_module (List.rev to_run);
    List.iter
      (* TODO: script_bis *)
        (fun (assertion : Woi.Types.assert_) ->
        match assertion with
        | Assert_trap_module (m, expected) -> begin
          let m = Woi.Simplify_bis.simplify m in
          let to_run, _link_state = Woi.Link_bis.link_module m link_state in
          match Woi.Interpret_bis.exec_module to_run with
          | exception Woi.Types.Trap msg -> assert (msg = expected)
          | () -> assert false
        end
        | _ -> failwith "TODO assertion" )
      assertions;
    ()
  end
  | Error e -> error e

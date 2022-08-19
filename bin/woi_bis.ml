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
    let () = Format.printf "simplified %i@." (List.length cmds) in
    let link_state = Woi.Link_bis.empty_state in
    let link_state =
      List.fold_left
        (fun state cmd ->
          match cmd with
          | `Module module_ -> Woi.Link_bis.link_module module_ state
          | `Register (name, id) -> Woi.Link_bis.register_module state ~name ~id
          )
        link_state cmds
    in
    ignore link_state;
    ()
  end
  | Error e -> error e

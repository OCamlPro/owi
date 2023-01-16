open Owi

let simplify_then_link_then_run file =
  let to_run, _link_state =
    List.fold_left
      (fun ((to_run, state) as acc) -> function
        | Types.Module m -> begin
          match Compile.until_link state m with
          | Ok (m, state) -> (m :: to_run, state)
          | Error msg -> failwith msg
        end
        | Types.Register (name, id) ->
          (to_run, Link.register_module state ~name ~id)
        | _ -> acc )
      ([], Link.empty_state) file
  in
  List.iter
    (fun m ->
      let res = Interpret.module_ m in
      Result.fold ~ok:Fun.id ~error:failwith res )
    (List.rev to_run)

let run_file exec filename =
  if not @@ Sys.file_exists filename then
    Log.err "file `%s` doesn't exist" filename;
  match Parse.from_file ~filename with
  | Ok script -> exec script
  | Error e -> Log.err "%s" e

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

let script =
  let doc = "run as a reference test suite script" in
  Cmdliner.Arg.(value & flag & info [ "script"; "s" ] ~doc)

let main debug script files =
  let exec =
    if script then Script.exec ~with_exhaustion:true
    else simplify_then_link_then_run
  in
  if debug then Log.debug_on := true;
  List.iter (run_file exec) files

let cli =
  let open Cmdliner in
  let doc = "OCaml WebAssembly Interpreter" in
  let man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ] in
  let info = Cmd.info "owi" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info Term.(const main $ debug $ script $ files)

let main () = exit @@ Cmdliner.Cmd.eval cli

let () = main ()

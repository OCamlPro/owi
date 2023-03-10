open Owi
open Syntax

let simplify_then_link_then_run file =
  let* to_run, _link_state =
    list_fold_left
      (fun ((to_run, state) as acc) instruction ->
        match instruction with
        | Types.Symbolic.Module m ->
          let* m, state = Compile.until_link state ~name:None m in
          Ok (m :: to_run, state)
        | Types.Symbolic.Register (name, id) ->
          let* state = Link.register_module state ~name ~id in
          Ok (to_run, state)
        | _ -> Ok acc )
      ([], Link.empty_state) file
  in
  list_iter Interpret.module_ (List.rev to_run)

let run_file exec filename =
  if not @@ Sys.file_exists filename then
    error_s "file `%s` doesn't exist" filename
  else
    let* script = Parse.from_file ~filename in
    exec script

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

let profiling =
  let doc = "profiling mode" in
  Cmdliner.Arg.(value & flag & info [ "profiling"; "p" ] ~doc)

let script =
  let doc = "run as a reference test suite script" in
  Cmdliner.Arg.(value & flag & info [ "script"; "s" ] ~doc)

let main profiling debug script files =
  let exec =
    if script then Script.exec ~with_exhaustion:true
    else simplify_then_link_then_run
  in
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let result = list_iter (run_file exec) files in
  match result with
  | Ok () -> ()
  | Error e ->
    Format.eprintf "%s@." e;
    exit 1

let cli =
  let open Cmdliner in
  let doc = "OCaml WebAssembly Interpreter" in
  let man = [ `S Manpage.s_bugs; `P "Email them to <contact@ndrs.fr>." ] in
  let info = Cmd.info "owi" ~version:"%%VERSION%%" ~doc ~man in
  Cmd.v info Term.(const main $ profiling $ debug $ script $ files)

let main () = exit @@ Cmdliner.Cmd.eval cli

let () = main ()

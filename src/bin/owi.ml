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

let exec, files =
  let exec = ref simplify_then_link_then_run in
  let files = ref [] in
  let spec =
    Arg.
      [ ( "--script"
        , Unit (fun () -> exec := Script.exec)
        , "run as a reference test suite script" )
      ; ("-s", Unit (fun () -> exec := Script.exec), "short for --script")
      ; ("--debug", Set Log.debug_on, "debug mode")
      ; ("-d", Set Log.debug_on, "short for --debug")
      ]
  in
  Arg.parse spec (fun s -> files := s :: !files) "wast interpreter %s <file>";
  (!exec, !files)

let run_file filename =
  if not @@ Sys.file_exists filename then
    Log.err "file `%s` doesn't exist" filename;
  match Parse.from_file ~filename with
  | Ok script -> exec script
  | Error e -> Log.err "%s" e

let () = List.iter run_file files

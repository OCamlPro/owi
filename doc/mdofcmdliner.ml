open Result.Syntax

type command = { words : string list }

let root = { words = [] }

let command_of_string s =
  match
    List.filter (fun s -> not @@ String.equal "" s) (String.split_on_char ' ' s)
  with
  | [] -> None
  | words -> Some { words }

let executable_name executable =
  let executable =
    if Fpath.has_ext ".exe" executable then Fpath.rem_ext executable
    else executable
  in
  Fpath.basename executable

let command_name executable_name { words } =
  String.concat " " (executable_name :: words)

let sanitize_filename word =
  String.map
    (function
      | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_') as c -> c | _ -> '-' )
    word

let filename_of_words words =
  words |> List.map sanitize_filename |> String.concat "-" |> Fpath.v
  |> Fpath.add_ext ".md"

let command_path executable_name command =
  filename_of_words (executable_name :: command.words)

let path_of_command output executable_name command =
  Fpath.(output // command_path executable_name command)

let rec is_prefix prefix words =
  match (prefix, words) with
  | [], _ -> true
  | prefix :: prefixes, word :: words when String.equal prefix word ->
    is_prefix prefixes words
  | _ -> false

let children commands command =
  let length = List.length command.words + 1 in

  List.filter
    (fun child ->
      List.length child.words = length && is_prefix command.words child.words )
    commands

let get_commands executable =
  let cmd = Bos.Cmd.(v "cmdliner" % "tool-commands" % p executable) in

  let* lines =
    Bos.OS.Cmd.(run_out cmd |> to_lines ~trim:true)
    |> Result.map_error (fun (`Msg msg) ->
      `Msg (Fmt.str "cmdliner tool-commands failed: %s" msg) )
  in

  Ok (List.filter_map command_of_string lines)

let run_help executable command =
  let cmd =
    Bos.Cmd.(v (p executable) %% of_list command.words % "--help=plain")
  in

  Bos.OS.Cmd.(run_out cmd |> to_string)
  |> Result.map_error (fun (`Msg msg) ->
    `Msg (Fmt.str "failed to get help: %s" msg) )

let addf buffer format = Fmt.kstr (Buffer.add_string buffer) format

let add_help buffer help =
  addf buffer "## Help\n\n```text\n%s" help;

  if not (String.ends_with ~suffix:"\n" help) then Buffer.add_char buffer '\n';

  Buffer.add_string buffer "```\n"

let add_command_link buffer executable_name command =
  addf buffer "- [`%s`](%a)\n"
    (command_name executable_name command)
    Fpath.pp
    (command_path executable_name command)

let rec add_children ~executable_name ~commands ~depth buffer command =
  children commands command
  |> List.iter (fun child ->
    for _ = 1 to depth do
      Buffer.add_string buffer "  "
    done;

    add_command_link buffer executable_name child;

    add_children ~executable_name ~commands ~depth:(depth + 1) buffer child )

let add_subcommands buffer executable_name commands command =
  match children commands command with
  | [] -> ()
  | _ ->
    Buffer.add_string buffer "\n## Subcommands\n\n";
    add_children ~executable_name ~commands ~depth:0 buffer command

let render_breadcrumb executable_name { words } =
  let buffer = Buffer.create 128 in

  let add_link label words =
    addf buffer "[%s](%a)" label Fpath.pp
      (filename_of_words (executable_name :: words))
  in

  add_link executable_name [];

  let rec loop prefix_rev = function
    | [] -> ()
    | word :: rest ->
      let prefix_rev = word :: prefix_rev in
      let prefix = List.rev prefix_rev in
      let label = command_name executable_name { words = prefix } in

      Buffer.add_string buffer " › ";

      if List.is_empty rest then addf buffer "**%s**" label
      else add_link label prefix;

      loop prefix_rev rest
  in

  loop [] words;
  Buffer.contents buffer

let render_page executable executable_name commands command =
  let* help = run_help executable command in

  let buffer = Buffer.create (String.length help + 1024) in

  Buffer.add_string buffer (render_breadcrumb executable_name command);

  addf buffer "\n\n# %s\n" (command_name executable_name command);

  add_subcommands buffer executable_name commands command;

  Buffer.add_char buffer '\n';
  add_help buffer help;

  Ok (Buffer.contents buffer)

let render_index executable executable_name commands =
  let* help = run_help executable root in

  let buffer = Buffer.create 4096 in

  addf buffer "# %s\n\n" executable_name;

  add_subcommands buffer executable_name commands root;

  Buffer.add_char buffer '\n';
  add_help buffer help;

  Ok (Buffer.contents buffer)

let write_page ~executable ~output ~executable_name ~commands command =
  let file = path_of_command output executable_name command in

  Logs.info (fun m -> m "Writing %a" Fpath.pp file);

  let* page = render_page executable executable_name commands command in

  Bos.OS.File.write file page

let write_index ~output ~executable_name contents =
  let file = path_of_command output executable_name root in

  Bos.OS.File.write file contents

let run ~executable ~output =
  let executable_name = executable_name executable in

  let* (true | false) = Bos.OS.Dir.create ~path:true ~mode:0o755 output in

  Logs.info (fun m -> m "Discovering commands from %a..." Fpath.pp executable);

  let* commands = get_commands executable in

  Logs.info (fun m -> m "Found %d commands." (List.length commands));

  let* index = render_index executable executable_name commands in

  let* () = write_index ~output ~executable_name index in

  let* () =
    List.fold_left
      (fun result command ->
        let* () = result in

        write_page ~executable ~output ~executable_name ~commands command )
      (Ok ()) commands
  in

  Logs.info (fun m -> m "Done. Documentation written to %a" Fpath.pp output);

  Ok ()

let setup_log =
  let open Cmdliner.Term.Syntax in
  let+ log_level =
    let env = Cmdliner.Cmd.Env.info "MDOFCMDLINER_VERBOSITY" in

    Logs_cli.level ~env ~docs:Cmdliner.Manpage.s_common_options ()
  and+ style_renderer =
    Fmt_cli.style_renderer ~docs:Cmdliner.Manpage.s_common_options ()
  in

  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level log_level;
  Logs.set_reporter (Logs_fmt.reporter ())

let path_conv = Cmdliner.Arg.conv (Fpath.of_string, Fpath.pp)

let executable =
  let doc = "Path to the executable." in

  Cmdliner.Arg.(
    required
    & opt (some path_conv) None
    & info [ "executable" ] ~docv:"FILE" ~doc )

let output =
  let doc = "Output directory." in

  Cmdliner.Arg.(
    required & opt (some path_conv) None & info [ "output" ] ~docv:"DIR" ~doc )

let cmd =
  let info =
    Cmdliner.Cmd.info "mdofcmdliner" ~version:"0.0" ~doc:""
      ~sdocs:Cmdliner.Manpage.s_common_options ~man:[]
      ~exits:Cmdliner.Cmd.Exit.defaults
  in

  let term =
    let open Cmdliner.Term.Syntax in
    let+ () = setup_log
    and+ executable
    and+ output in

    run ~executable ~output
  in

  Cmdliner.Cmd.v info term

type outcome = (unit, [ Cmdliner.Cmd.eval_error | `Msg of string ]) Result.t

let exit_code_of_outcome = function
  | Ok () -> Cmdliner.Cmd.Exit.ok
  | Error (`Term | `Parse | `Msg _) -> Cmdliner.Cmd.Exit.cli_error
  | Error `Exn -> Cmdliner.Cmd.Exit.internal_error

let print_outcome = function
  | Ok () -> Logs.app (fun m -> m "OK!")
  | Error (`Msg msg) -> Logs.err (fun m -> m "%s" msg)
  | Error `Exn -> Logs.err (fun m -> m "unhandled exception")
  | Error (`Term | `Parse) -> Logs.err (fun m -> m "command line parsing error")

let () =
  let outcome =
    match Cmdliner.Cmd.eval_value cmd with
    | Ok (`Help | `Version) -> Ok ()
    | Ok (`Ok result) -> (result :> outcome)
    | Error _ as result -> (result :> outcome)
  in

  print_outcome outcome;
  exit (exit_code_of_outcome outcome)

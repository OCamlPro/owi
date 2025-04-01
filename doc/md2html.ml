let existing_file_conv =
  let parse s =
    match Fpath.of_string s with
    | Error _ as e -> e
    | Ok path -> begin
      match Bos.OS.File.exists path with
      | Ok true -> Ok path
      | Ok false -> Fmt.error_msg "no file '%a'" Fpath.pp path
      | Error _ as e -> e
    end
  in
  Cmdliner.Arg.conv (parse, Fpath.pp)

let source_file =
  let doc = "source file" in
  Cmdliner.Arg.(
    required & pos 0 (some existing_file_conv) None & info [] ~doc ~docv:"FILE" )

let info =
  let doc = "Markdown to HTML documentation builder" in
  let sdocs = Cmdliner.Manpage.s_common_options in
  let man = [] in
  Cmdliner.Cmd.info "md2html" ~doc ~sdocs ~man

let cmd =
  let open Cmdliner.Term.Syntax in
  let+ source_file in
  Fmt.pr "file: %a" Fpath.pp source_file

let cli = Cmdliner.Cmd.v info cmd

let () =
  match Cmdliner.Cmd.eval_value cli with
  | Ok (`Help | `Version) -> ()
  | Ok (`Ok ()) -> ()
  | Error `Parse -> assert false
  | Error `Term -> assert false
  | Error `Exn -> assert false

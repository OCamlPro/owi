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

  let chan = open_in file in

  let lexbuf = Sedlexing.Utf8.from_channel chan in

  let script = Woi.Handle.process lexbuf in

  close_in chan;

  Woi.Check.script script;

  Woi.Debug.debug Format.err_formatter "%a\n%!" Woi.Pp.file script;

  let script, modules = Woi.Simplify.script script in

  Woi.Interpret.exec script modules

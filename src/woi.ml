let error msg =
  Format.eprintf "error: %s@." msg;
  exit 1

let () =
  if Array.length Sys.argv <> 2 then
    error (Format.sprintf "usage: %s <file>" Sys.argv.(0));

  let file = Sys.argv.(1) in

  if not @@ Sys.file_exists file then
    error (Format.sprintf "file `%s` doesn't exist" file);

  let chan = open_in file in

  let lexbuf = Sedlexing.Utf8.from_channel chan in

  Woi.Handle.process lexbuf;

  close_in chan

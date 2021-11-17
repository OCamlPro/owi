let test_file f =
  Format.printf "testing file: `%a`@." Fpath.pp f;
  match Bos.OS.File.read f with
  | Ok s ->
    let lexbuf = Sedlexing.Utf8.from_string s in
    let _ast = Woi.Handle.process lexbuf in
    ()
  | Error (`Msg e) ->
    Format.eprintf "error: %s@." e;
    exit 1

let () =
  match Bos.OS.Dir.contents ~rel:false Fpath.(v "reference") with
  | Ok l -> List.iter test_file l
  | Error (`Msg e) ->
    Format.eprintf "error: %s@." e;
    exit 1

let test_file f =
  Format.printf "testing file: `%a`@." Fpath.pp f;
  match Bos.OS.File.read f with
  | Ok s ->
    let lexbuf = Sedlexing.Utf8.from_string s in
    Woi.Handle.process lexbuf
  | Error (`Msg e) -> begin
      Format.eprintf "error: %s@." e;
      exit 1
    end

let () =
  match Bos.OS.Dir.contents ~rel:false Fpath.(v "reference") with
  | Ok l -> List.iter test_file l
  | Error (`Msg e) ->
    begin
      Format.eprintf "error: %s@." e;
      exit 1
    end

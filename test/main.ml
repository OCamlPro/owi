let test_file f =
  Format.printf "testing file     : `%a`... " Fpath.pp f;
  match Bos.OS.File.read f with
  | Ok s -> begin
    try
      let lexbuf = Sedlexing.Utf8.from_string s in
      let script = Woi.Handle.process lexbuf in
      Woi.Check.script script;
      let script, modules = Woi.Simplify.script script in
      Woi.Interpret.exec script modules;
      Format.printf "OK !@.";
      Ok ()
    with
    | Failure s ->
      Format.printf "FAILED !@.";
      Error s
    | _e ->
      Format.printf "FAILED ! @.";
      Error ""
  end
  | Error (`Msg e) ->
    Format.eprintf "error     : %s@." e;
    Error e

let test_directory d =
  let count_error = ref 0 in
  Format.printf "testing directory: `%a`@." Fpath.pp d;
  match Bos.OS.Dir.contents ~rel:false d with
  | Ok l ->
    List.iter
      (fun file ->
        match test_file file with
        | Ok () -> ()
        | Error _e -> incr count_error )
      l;
    if !count_error > 0 then
      Error (Format.sprintf "%d test failed !" !count_error)
    else
      Ok ()
  | Error (`Msg e) ->
    Format.eprintf "error      : %s@." e;
    Error e

let () =
  let has_error = ref false in
  begin
    match test_directory Fpath.(v "passing") with
    | Ok () -> ()
    | Error e ->
      Format.eprintf "error            : %s@." e;
      has_error := true
  end;
  begin
    match test_directory Fpath.(v "reference") with
    | Ok () -> ()
    | Error e ->
      Format.eprintf "error: %s@." e;
      has_error := true
  end;
  if !has_error then exit 1

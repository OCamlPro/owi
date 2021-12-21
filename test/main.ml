let count_total = ref 0

let count_total_failed = ref 0

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
    (* TODO: do not catch Empty *)
    | Woi.Stack.Empty ->
      let msg = "empty stack :S" in
      Format.printf "FAILED: `%s` !@." msg;
      Error msg
    | Failure s
    | Invalid_argument s ->
      Format.printf "FAILED: `%s` !@." s;
      Error s
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
        incr count_total;
        match test_file file with
        | Ok () -> ()
        | Error _e ->
          incr count_error;
          incr count_total_failed )
      (List.sort compare l);
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
  Format.printf "results : %d / %d !@."
    (!count_total - !count_total_failed)
    !count_total;
  if !has_error then exit 1

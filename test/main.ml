module Script = Woi.Script_bis

let count_total = ref 0

let count_total_failed = ref 0

let pp_red fmt s = Format.fprintf fmt "\x1b[31m%s\x1b[0m" s

let pp_green fmt s = Format.fprintf fmt "\x1b[32m%s\x1b[0m" s

let test_file f =
  Format.printf "testing file     : `%a`... " Fpath.pp f;
  match Woi.Parse.from_file ~filename:(Fpath.to_string f) with
  | Ok script -> begin
    try
      Script.exec script;
      Format.printf "%a !@." pp_green "OK";
      Ok ()
    with
    | Assert_failure (s, _, _)
    | Woi.Types.Trap s
    | Failure s
    | Invalid_argument s
    ->
      Format.printf "%a: `%s` !@." pp_red "FAILED"
        (String.concat " " @@ String.split_on_char '\n' s);
      Error s
  end
  | Error e -> Error e

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
    else Ok ()
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

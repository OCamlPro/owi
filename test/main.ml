let count_total = ref 0

let count_total_failed = ref 0

let pp_red fmt s = Format.fprintf fmt "\x1b[31m%s\x1b[0m" s

let pp_green fmt s = Format.fprintf fmt "\x1b[32m%s\x1b[0m" s

let pp_ok fmt = Format.fprintf fmt "%a !@." pp_green "OK"

let pp_error fmt msg = Format.fprintf fmt "%a: %s !@." pp_red "FAILED" msg

let test_file ~optimize f =
  Format.printf "testing %s: `%a`... "
    (if optimize then "optimized file" else "file          ")
    Fpath.pp f;
  match Owi.Parse.from_file ~filename:(Fpath.to_string f) with
  | Ok script -> begin
    match Owi.Script.exec script ~optimize with
    | Ok () as ok ->
      pp_ok Format.std_formatter;
      ok
    | Error msg as error ->
      pp_error Format.std_formatter msg;
      error
  end
  | Error msg as e ->
    let msg = String.concat " | " @@ String.split_on_char '\n' msg in
    pp_error Format.std_formatter msg;
    e

let test_directory d =
  let count_error = ref 0 in
  Format.printf "testing directory     : `%a`@." Fpath.pp d;
  match Bos.OS.Dir.contents ~rel:false d with
  | Ok l ->
    List.iter
      (fun file ->
        incr count_total;
        begin
          match test_file ~optimize:false file with
          | Ok () -> ()
          | Error _e ->
            incr count_error;
            incr count_total_failed
        end;
        incr count_total;
        begin
          match test_file ~optimize:true file with
          | Ok () -> ()
          | Error _e ->
            incr count_error;
            incr count_total_failed
        end )
      (List.sort compare l);
    if !count_error > 0 then
      Error (Format.sprintf "%d test failed" !count_error)
    else Ok ()
  | Error (`Msg e) ->
    pp_error Format.std_formatter e;
    Error e

let () =
  let has_error = ref false in
  begin
    match test_directory Fpath.(v "passing") with
    | Ok () -> ()
    | Error e ->
      pp_error Format.std_formatter e;
      has_error := true
  end;
  begin
    match test_directory Fpath.(v "reference") with
    | Ok () -> ()
    | Error e ->
      pp_error Format.std_formatter e;
      has_error := true
  end;
  if Option.is_some @@ Sys.getenv_opt "OWIGC" then begin
    match test_directory Fpath.(v "gc") with
    | Ok () -> ()
    | Error e ->
      pp_error Format.std_formatter e;
      has_error := true
  end;
  Format.printf "results : %d / %d !@."
    (!count_total - !count_total_failed)
    !count_total;
  if !has_error then exit 1

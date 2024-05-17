module Unix = struct
  include Unix
  include Bos.OS.U
end

let ( let* ) o f = match o with Ok v -> f v | Error _ as e -> e

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let object_field (yml : Yaml.value) (field : string) =
  match yml with
  | `O l ->
    List.assoc_opt field l
    |> Option.to_result ~none:(`Msg (Format.sprintf "missing field %s" field))
  | _ -> Error (`Msg "malformed yaml")

let array_map f (yml : Yaml.value) =
  match yml with
  | `A l -> Ok (List.filter_map (fun v -> f v |> Result.to_option) l)
  | _ -> Error (`Msg "malformed yaml")

let property (yml : Yaml.value) =
  let* file = object_field yml "property_file" in
  let* expected = object_field yml "expected_verdict" in
  let* file = Yaml.Util.to_string file in
  let file = Fpath.v file in
  let+ expected = Yaml.Util.to_bool expected in
  (file, expected)

let problem (yml : Yaml.value) dir =
  let* input_file = object_field yml "input_files" in
  let* properties = object_field yml "properties" in
  let* options = object_field yml "options" in
  let* language = object_field options "language" in
  let* language = Yaml.Util.to_string language in
  let* properties = array_map property properties in
  let+ input_file = Yaml.Util.to_string input_file in
  let input_file = Fpath.(dir / input_file) in
  (input_file, properties, language)

let parse_file path =
  let* yml =
    Bos.OS.File.with_ic path
      (fun chan () ->
        let s = In_channel.input_all chan in
        Yaml.of_string s )
      ()
  in
  yml

let problems_root = Fpath.v "testcomp/sv-benchmarks/c/"

let output_dir =
  let t = Unix.localtime @@ Unix.gettimeofday () in
  let filename =
    Format.sprintf "testcomp-results-%d-%02d-%02d_%02dh%02dm%02ds/"
      (1900 + t.tm_year) (1 + t.tm_mon) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  in
  Fpath.v filename

let timeout =
  if Array.length Sys.argv >= 2 then float_of_string Sys.argv.(1) else 30.

let is_in_whitelist =
  let tbl = Hashtbl.create 2048 in
  String.split_on_char '\n' Whitelist.v
  |> List.iter (function
       | "" -> ()
       | file ->
         let file = Fpath.(problems_root // v file) in
         Hashtbl.replace tbl file () );
  fun file -> Hashtbl.mem tbl file

let is_valid_problem language properties =
  language = "C"
  && List.exists
       (function
         | file, false -> String.equal "unreach-call.prp" (Fpath.filename file)
         | _ -> false )
       properties

let files =
  let* res =
    Bos.OS.Dir.fold_contents ~dotfiles:false ~elements:`Files ~traverse:`Any
      (fun name acc ->
        let* acc in
        if not (Fpath.has_ext ".yml" name && is_in_whitelist name) then Ok acc
        else
          let* yml = parse_file name in
          let+ input_file, properties, language =
            let dir = fst @@ Fpath.split_base name in
            problem yml dir
          in
          if is_valid_problem language properties then input_file :: acc
          else acc )
      (Ok []) problems_root
  in
  res

type rusage =
  { clock : float
  ; utime : float
  ; stime : float
  }

type process_result =
  | Timeout of rusage
  | Nothing of rusage
  | Reached of rusage
  | Other of int * rusage
  | Killed of rusage

exception Sigchld

let wait_pid =
  let last_utime = ref 0. in
  let last_stime = ref 0. in
  fun pid ->
    let did_timeout = ref false in
    let start_time = Unix.gettimeofday () in
    begin
      try
        Sys.set_signal Sys.sigchld
          (Signal_handle (fun (_ : int) -> raise Sigchld));
        Unix.sleepf timeout;
        did_timeout := true;
        Unix.kill pid 9;
        Sys.set_signal Sys.sigchld Signal_default
      with Sigchld -> ()
    end;
    Sys.set_signal Sys.sigchld Signal_default;
    let waited_pid, status = Unix.waitpid [] pid in
    let end_time = Unix.gettimeofday () in
    let { Rusage.utime; stime; _ } = Rusage.get Rusage.Children in
    assert (waited_pid = pid);

    let utime_diff = utime -. !last_utime in
    let stime_diff = stime -. !last_stime in
    last_utime := utime;
    last_stime := stime;
    let utime = utime_diff in
    let stime = stime_diff in
    let clock = end_time -. start_time in

    let rusage = { clock; utime; stime } in
    if !did_timeout then Timeout rusage
    else
      match status with
      | WEXITED code ->
        if code = 0 then Nothing rusage
        else if code = 13 then Reached rusage
        else Other (code, rusage)
      | WSIGNALED _ | WSTOPPED _ -> Killed rusage

let run_on_file ~out_dir file =
  let dup ~src ~dst =
    let new_file =
      Unix.openfile
        Fpath.(out_dir / dst |> to_string)
        [ O_CREAT; O_WRONLY ] 0o666
    in
    Unix.dup2 new_file src;
    Unix.close new_file
  in
  dup ~dst:"stdout" ~src:Unix.stdout;
  dup ~dst:"stderr" ~src:Unix.stderr;
  let out_dir = Fpath.(out_dir / "owi") |> Fpath.to_string in
  let file = Fpath.to_string file in
  Unix.execvp "owi" [| "owi"; "c"; "--unsafe"; "-O1"; "-o"; out_dir; file |]

let fork_and_run_on_file i file =
  let out_dir = Fpath.(output_dir / string_of_int i) in
  let+ () = Unix.mkdir out_dir 0o777 in
  let pid = Unix.fork () in
  let result = if pid = 0 then run_on_file ~out_dir file else wait_pid pid in
  begin
    match result with
    | Timeout t ->
      Format.printf "Timeout in %g %g %g@\n" t.clock t.utime t.stime
    | Nothing t ->
      Format.printf "Nothing in %g %g %g@\n" t.clock t.utime t.stime
    | Reached t ->
      Format.printf "Reached in %g %g %g@\n" t.clock t.utime t.stime
    | Other (code, t) ->
      Format.printf "Other %i in %g %g %g@\n" code t.clock t.utime t.stime
    | Killed t -> Format.printf "Killed in %g %g %g@\n" t.clock t.utime t.stime
  end;
  result

let quick_results results =
  let nothing = ref 0 in
  let reached = ref 0 in
  let timeout = ref 0 in
  let bad = ref 0 in
  List.iter
    (fun (_problem, result) ->
      match result with
      | Nothing _ -> incr nothing
      | Reached _ -> incr reached
      | Timeout _ -> incr timeout
      | Other _ | Killed _ -> incr bad )
    results;
  Format.printf "Nothing: %6i    Reached: %6i    Timeout: %6i    Bad: %6i"
    !nothing !reached !timeout !bad

let res =
  let* files in
  let files = List.sort Fpath.compare files in
  let* () = Bos.OS.Dir.delete ~must_exist:false ~recurse:true output_dir in
  let+ (_existed : bool) =
    Bos.OS.Dir.create ~path:true ~mode:0o755 output_dir
  in
  let len = List.length files in
  let results = ref [] in
  List.iteri
    (fun i input_file ->
      let i = succ i in
      Format.printf "Run %d/%d: %a@\n  @[<v>" i len Fpath.pp input_file;
      let result = fork_and_run_on_file i input_file in
      match result with
      | Error (`Unix e) -> failwith (Unix.error_message e)
      | Ok result ->
        results := (problem, result) :: !results;
        quick_results !results;
        Format.printf "@]@\n%!" )
    files

let () =
  match res with
  | Ok () -> ()
  | Error (`Msg m) ->
    Format.eprintf "ERROR: %s@\n" m;
    exit 1

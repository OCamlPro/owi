module Unix = struct
  include Unix
  include Bos.OS.U
end

let tool =
  Tool.mk_owi ~concolic:false ~workers:24 ~optimisation_level:3
    ~solver:Smtml.Solver_type.Z3_solver

let _tool = Tool.mk_klee ()

let reference_name = Tool.to_reference_name tool

let ( let* ) o f = match o with Ok v -> f v | Error _ as e -> e

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let ok_or_fail = function
  | Error (`Msg m) ->
    Format.eprintf "ERROR: %s@\n" m;
    exit 1
  | Error (`Unix e) ->
    Format.eprintf "ERROR: %s@\n" (Unix.error_message e);
    exit 1
  | Ok x -> x

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

let timeout =
  if Array.length Sys.argv >= 2 then float_of_string Sys.argv.(1) else 30.

let output_dir =
  let t = Unix.localtime @@ Unix.gettimeofday () in
  let filename =
    Format.sprintf "results-testcomp-%s-%d-%02d-%02d_%02dh%02dm%02ds/"
      reference_name (1900 + t.tm_year) (1 + t.tm_mon) t.tm_mday t.tm_hour
      t.tm_min t.tm_sec
  in
  Fpath.v filename

let (_existed : bool) =
  Bos.OS.Dir.create ~path:true ~mode:0o755 output_dir |> ok_or_fail

let output_chan = Fpath.(output_dir / "results") |> Fpath.to_string |> open_out

let fmt = Format.formatter_of_out_channel output_chan

let pp x = Format.fprintf fmt x

let runs =
  let+ files in
  let files = List.sort Fpath.compare files in
  let len = List.length files in
  let results = ref Report.Runs.empty in
  let limit = 10_000 in
  List.iteri
    (fun i file ->
      let i = succ i in
      if i < limit + 1 then begin
        pp "%a@\n  @[<v>" (Report.Run.pp_header (min len limit)) (i, file);
        let result =
          Tool.fork_and_run_on_file ~i ~fmt ~output_dir ~file ~tool ~timeout
          |> ok_or_fail
        in
        let result = { Report.Run.i; file; res = result } in
        results := Report.Runs.add result !results;
        pp "%a@]@\n%!" Report.Runs.pp_quick_results !results
      end )
    files;
  !results

let notify_finished runs =
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let headers =
    let headers = Header.init () in
    Header.add_list headers
      [ ("Content-type", "application/json"); ("User-Agent", "Owibot/1.1") ]
  in
  let send url body =
    let body = Cohttp_lwt.Body.of_string (Yojson.to_string body) in
    Client.post ~body ~headers url
  in
  let head () =
    let open Bos in
    let cmd = Cmd.(v "git" % "rev-parse" % "--short" % "HEAD") in
    let output = OS.Cmd.run_out ~err:OS.Cmd.err_run_out cmd in
    match OS.Cmd.out_string ~trim:true output with
    | Ok (stdout, (_, `Exited 0)) -> stdout
    | Error (`Msg err) ->
      Format.eprintf "ERROR: %s@." err;
      "unknown"
    | Ok (stdout, (_, (`Exited _ | `Signaled _))) ->
      Format.eprintf "%s@\nWARN: Unable to fetch git HEAD@." stdout;
      "unknown"
  in
  let text =
    Format.asprintf
      "@[<v>Using:@;\
       - Tool: `%s`@;\
       - Timeout: `%F`@;\
       - Output dir: `%a`@]@\n\
       @\n\
       Results:@\n\
       @\n\
       %a@\n\
       @\n\
       Time stats (in seconds):@\n\
       @\n\
       %a@."
      reference_name timeout Fpath.pp output_dir Report.Runs.pp_table_results
      runs Report.Runs.pp_table_statistics runs
  in
  (* Notify on `ZULIP_WEBHOOK` *)
  match Bos.OS.Env.var "ZULIP_WEBHOOK" with
  | None -> Format.eprintf "%s" text
  | Some url ->
    let url = Uri.of_string url in
    let title =
      Format.sprintf "Benchmark results (commit hash=%s) :octopus:" (head ())
    in
    let body =
      (* Using Yojson just to ensure we're sending correct json *)
      `Assoc
        [ ( "blocks"
          , `List
              [ `Assoc
                  [ ("type", `String "header")
                  ; ( "text"
                    , `Assoc
                        [ ("type", `String "plain_text")
                        ; ("text", `String title)
                        ; ("emoji", `Bool true)
                        ] )
                  ]
              ; `Assoc
                  [ ("type", `String "section")
                  ; ( "text"
                    , `Assoc
                        [ ("type", `String "mrkdwn"); ("text", `String text) ]
                    )
                  ]
              ] )
        ]
    in
    let result, _ = Lwt_main.run @@ send url body in
    let status = Response.status result in
    Format.eprintf "Server responded: %s@." (Code.string_of_status status)

let () =
  let runs = ok_or_fail runs in
  notify_finished runs;
  Report.Gen.full_report runs output_dir reference_name |> ok_or_fail

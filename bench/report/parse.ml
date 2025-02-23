let ( let* ) o f = match o with Ok v -> f v | Error _ as e -> e

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let ksprintf = Format.ksprintf

let error = Result.error

let from_file file =
  let file = Fpath.to_string file in

  let chan = open_in file in

  let runs = ref [] in

  let parse_file () =
    try
      while true do
        let run = input_line chan in
        let result = input_line chan in
        let summary = input_line chan in
        runs := (run, result, summary) :: !runs
      done
    with End_of_file -> runs := List.rev !runs
  in

  Fun.protect ~finally:(fun () -> close_in chan) parse_file;

  let runs = !runs in

  let parse_float s =
    match float_of_string_opt s with
    | None -> ksprintf error "malformed float %S" s
    | Some f -> Ok f
  in

  let parse_int64 s =
    match Int64.of_string_opt s with
    | None -> ksprintf error "malformed int64 %S" s
    | Some i -> Ok i
  in

  let parse_rusage t1 t2 t3 rss =
    let* clock = parse_float t1 in
    let* utime = parse_float t2 in
    let* stime = parse_float t3 in
    let+ maxrss = parse_int64 rss in
    { Rusage.clock; utime; stime; maxrss }
  in

  let parse_int s =
    match int_of_string_opt s with
    | None -> ksprintf error "malformed int %S" s
    | Some i -> Ok i
  in

  let rm_empty_str l = List.filter (fun s -> s <> "") l in

  let parse_run (run, result, _summary) =
    let* counter, file =
      match String.split_on_char ' ' run |> rm_empty_str with
      | [ "Run"; counter; file ] -> Ok (counter, Fpath.v file)
      | _ -> ksprintf error "malformed run: %S" run
    in
    let* i =
      match String.split_on_char '/' counter |> rm_empty_str with
      | [ i; _total ] -> parse_int i
      | _ -> ksprintf error "malformed counter: %S" counter
    in
    let+ res =
      match String.split_on_char ' ' result |> rm_empty_str with
      | [ "Reached"; "in"; t1; t2; t3; rss ] ->
        let+ rusage = parse_rusage t1 t2 t3 rss in
        Run_result.Reached rusage
      | [ "Timeout"; "in"; t1; t2; t3; rss ] ->
        let+ rusage = parse_rusage t1 t2 t3 rss in
        Run_result.Timeout rusage
      | [ "Other"; n; "in"; t1; t2; t3; rss ] ->
        let* n = parse_int n in
        let+ rusage = parse_rusage t1 t2 t3 rss in
        Run_result.Other (rusage, n)
      | [ "Nothing"; "in"; t1; t2; t3; rss ] ->
        let+ rusage = parse_rusage t1 t2 t3 rss in
        Run_result.Nothing rusage
      | [ "Signaled"; n; "in"; t1; t2; t3; rss ] ->
        let* rusage = parse_rusage t1 t2 t3 rss in
        let+ n = parse_int n in
        Run_result.Stopped (rusage, n)
      | [ "Stopped"; n; "in"; t1; t2; t3; rss ] ->
        let* rusage = parse_rusage t1 t2 t3 rss in
        let+ n = parse_int n in
        Run_result.Signaled (rusage, n)
      | _ -> ksprintf error "malformed result: %S" result
    in
    { Run.i; res; file }
  in

  List.fold_left
    (fun runs v ->
      match parse_run v with
      | Error e -> failwith e
      | Ok run -> Runs.add run runs )
    Runs.empty runs

type t =
  | Owi of
      { concolic : bool
      ; optimisation_level : int
      ; workers : int
      ; solver : Smtml.Solver_type.t
      }
  | Klee
  | Symbiotic

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let to_short_name = function
  | Owi _ -> "owi"
  | Klee -> "klee"
  | Symbiotic -> "symbiotic"

let to_reference_name = function
  | Owi { concolic; workers; optimisation_level; solver } ->
    Format.asprintf "owi_w%d_O%d_s%a%s" workers optimisation_level
      Smtml.Solver_type.pp solver
      (if concolic then "_concolic" else "")
  | Klee -> "klee"
  | Symbiotic -> "symbiotic"

let mk_owi ~concolic ~workers ~optimisation_level ~solver =
  Owi { concolic; workers; optimisation_level; solver }

let mk_klee () = Klee

let mk_symbiotic () = Symbiotic

exception Sigchld

let kill_klee_descendants () =
  let _ = Format.ksprintf Sys.command "pkill klee" in
  ()

let wait_pid =
  let last_utime = ref 0. in
  let last_stime = ref 0. in
  fun ~pid ~timeout ~tool ~dst_stderr ->
    let did_timeout = ref false in
    let start_time = Unix.gettimeofday () in
    begin
      try
        Sys.set_signal Sys.sigchld
          (Signal_handle (fun (_ : int) -> raise Sigchld));
        Unix.sleepf timeout;
        did_timeout := true;
        (* we kill the process group id (pgid) which should be equal to pid *)
        Unix.kill (-pid) 9;
        Sys.set_signal Sys.sigchld Signal_default
      with Sigchld -> ()
    end;
    Sys.set_signal Sys.sigchld Signal_default;
    let waited_pid, status = Unix.waitpid [] (-pid) in
    (* Because symbiotic is leaking klee processes *)
    kill_klee_descendants ();
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

    (* Sometimes the clock goes a little bit above the allowed timeout... *)
    let clock = min clock timeout in
    let rusage = { Report.Rusage.clock; utime; stime } in

    if !did_timeout || Float.equal clock timeout then
      Report.Run_result.Timeout rusage
    else
      match status with
      | WEXITED code -> begin
        match tool with
        | Owi _ ->
          if code = 0 then Nothing rusage
          else if code = 13 then Reached rusage
          else Other (rusage, code)
        | Klee ->
          if code = 0 then begin
            let chan = open_in (Fpath.to_string dst_stderr) in
            let has_found_error = ref false in
            begin
              try
                while true do
                  let line = input_line chan in
                  match
                    String.split_on_char ' ' line
                    |> List.filter (fun s -> s <> "")
                  with
                  | [ "KLEE:"; "ERROR:"; _location; "ASSERTION"; "FAIL:"; "0" ]
                    ->
                    has_found_error := true;
                    raise Exit
                  | _line -> ()
                done
              with End_of_file | Exit -> ()
            end;
            close_in chan;
            if !has_found_error then Reached rusage else Nothing rusage
          end
          else Other (rusage, code)
        | Symbiotic ->
          if code = 0 then begin
            match Bos.OS.File.read dst_stderr with
            | Error (`Msg err) -> failwith err
            | Ok data -> (
              let error = Astring.String.find_sub ~sub:"Found ERROR!" data in
              match error with
              | Some _ -> Reached rusage
              | None -> Nothing rusage )
          end
          else Other (rusage, code)
      end
      | WSIGNALED n -> Signaled (rusage, n)
      | WSTOPPED n -> Stopped (rusage, n)

let execvp ~output_dir tool file timeout =
  let output_dir = Fpath.(output_dir / to_short_name tool) |> Fpath.to_string in
  let file = Fpath.to_string file in
  let timeout = string_of_int timeout in
  let bin, args =
    match tool with
    | Owi { workers; optimisation_level; concolic; solver } ->
      ( "owi"
      , [ "owi"; "c" ]
        @ (if concolic then [ "--concolic" ] else [])
        @ [ "--unsafe"
          ; "-vv"
          ; "--fail-on-assertion-only"
          ; Format.sprintf "-O%d" optimisation_level
          ; Format.sprintf "-w%d" workers
          ; "--workspace"
          ; output_dir
          ; "--solver"
          ; Format.asprintf "%a" Smtml.Solver_type.pp solver
          ; file
          ] )
    | Klee ->
      let path_to_klee = "klee/bin/klee" in
      ( path_to_klee
      , [ path_to_klee
        ; "--error-only"
        ; "--max-time"
        ; timeout
        ; "--max-walltime"
        ; timeout
        ; file
        ] )
    | Symbiotic ->
      let path_to_symbiotic = "symbiotic/bin/symbiotic" in
      ( path_to_symbiotic
      , [ path_to_symbiotic
        ; "--test-comp"
        ; Format.sprintf "--timeout=%s" timeout
        ; "--prp=testcomp/sv-benchmarks/c/properties/coverage-error-call.prp"
        ; file
        ] )
  in
  let args = Array.of_list args in
  Unix.execvp bin args

let dup ~src ~dst =
  let new_file =
    Unix.openfile (Fpath.to_string dst) [ O_CREAT; O_WRONLY ] 0o666
  in
  Unix.dup2 new_file src;
  Unix.close new_file

let move_query_log_to_output_dir ~(output_dir : Fpath.t) : unit =
  let src = Fpath.v (Smtml.Tmp_log_path.get ()) in
  let dst = Fpath.(output_dir / "queries_log.jsonl") in
  match Bos.OS.File.exists src with
  | Ok true -> (
      match Bos.OS.Path.move src dst with
      | Ok () -> ()
      | Error (`Msg e) -> Logs.err (fun m -> m "move failed: %s" e)
    )
  | Ok false ->
    Logs.err (fun m -> m "Query log not found at %a" Fpath.pp src)
  | Error (`Msg msg) ->
    Logs.err (fun m -> m "Error checking query log file: %s" msg)

let fork_and_run_on_file ~i ~fmt ~output_dir ~file ~tool ~timeout =
  let output_dir = Fpath.(output_dir / string_of_int i) in
  let+ (_existed : bool) =
    Bos.OS.Dir.create ~path:true ~mode:0o755 output_dir
  in
  let dst_stderr = Fpath.(output_dir / "stderr") in
  let result =
    let rec loop retries =
      let pid = Unix.fork () in
      let tmp_log_path =
        let pidt = Unix.getpid () in
        Fmt.str "/home/intern-fw-03/Documents/queries/queries_log_%d.jsonl" pidt
      in
      if pid = 0 then begin
        Smtml.Tmp_log_path.set tmp_log_path;
        ExtUnix.Specific.setpgid 0 0;
        dup ~dst:Fpath.(output_dir / "stdout") ~src:Unix.stdout;
        dup ~dst:dst_stderr ~src:Unix.stderr;
        execvp ~output_dir tool file (int_of_float timeout)
      end
      else begin
        Smtml.Tmp_log_path.set tmp_log_path;
        match wait_pid ~pid ~timeout ~tool ~dst_stderr with
        | (Signaled _ | Stopped _) as result ->
          if retries = 0 then result else loop (pred retries)
        | result -> result
      end
    in
    loop 10
  in
  move_query_log_to_output_dir ~output_dir;
  Format.fprintf fmt "%a@\n" Report.Run_result.pp result;
  result

type t =
  | Owi of
      { workers : int
      ; optimisation_level : int
      }
  | Klee

let ( let+ ) o f = match o with Ok v -> Ok (f v) | Error _ as e -> e

let to_short_name = function Owi _ -> "owi" | Klee -> "klee"

let to_reference_name = function
  | Owi { workers; optimisation_level } ->
    Format.sprintf "owi_w%d_O%d" workers optimisation_level
  | Klee -> "klee"

let mk_owi ~workers ~optimisation_level = Owi { workers; optimisation_level }

let mk_klee () = Klee

exception Sigchld

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

    let rusage = { Report.Rusage.clock; utime; stime } in
    if !did_timeout then Report.Run_result.Timeout rusage
    else
      match status with
      | WEXITED code -> begin
        match tool with
        | Owi _ ->
          if code = 0 then Nothing rusage
          else if code = 13 then Reached rusage
          else Other (code, rusage)
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
          else Other (code, rusage)
      end
      | WSIGNALED _ | WSTOPPED _ -> Killed rusage

let execvp ~output_dir tool file timeout =
  let output_dir = Fpath.(output_dir / to_short_name tool) |> Fpath.to_string in
  let file = Fpath.to_string file in
  let timeout = string_of_int timeout in
  let bin, args =
    match tool with
    | Owi { workers; optimisation_level } ->
      ( "owi"
      , [| "owi"
         ; "c"
         ; "--unsafe"
         ; Format.sprintf "-O%d" optimisation_level
         ; Format.sprintf "-w%d" workers
         ; "-o"
         ; output_dir
         ; file
        |] )
    | Klee ->
      let path_to_klee = "klee/bin/klee" in
      ( path_to_klee
      , [| path_to_klee
         ; "--error-only"
         ; "--max-time"
         ; timeout
         ; "--max-walltime"
         ; timeout
         ; file
        |] )
  in
  Unix.execvp bin args

let dup ~src ~dst =
  let new_file =
    Unix.openfile (Fpath.to_string dst) [ O_CREAT; O_WRONLY ] 0o666
  in
  Unix.dup2 new_file src;
  Unix.close new_file

let fork_and_run_on_file ~i ~fmt ~output_dir ~file ~tool ~timeout =
  let output_dir = Fpath.(output_dir / string_of_int i) in
  let+ (_existed : bool) =
    Bos.OS.Dir.create ~path:true ~mode:0o755 output_dir
  in
  let dst_stderr = Fpath.(output_dir / "stderr") in
  let pid = Unix.fork () in
  let result =
    if pid = 0 then begin
      ExtUnix.Specific.setpgid 0 0;
      dup ~dst:Fpath.(output_dir / "stdout") ~src:Unix.stdout;
      dup ~dst:dst_stderr ~src:Unix.stderr;
      execvp ~output_dir tool file (int_of_float timeout)
    end
    else begin
      wait_pid ~pid ~timeout ~tool ~dst_stderr
    end
  in
  Format.fprintf fmt "%a@\n" Report.Run_result.pp result;
  result

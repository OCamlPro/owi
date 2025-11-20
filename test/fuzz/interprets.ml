let ( let* ) = Result.bind

exception Timeout

module type INTERPRET = sig
  val parse_and_run : Owi.Text.Module.t -> (unit, Owi.Result.err) Result.t

  val name : string
end

let unset () = Sys.set_signal Sys.sigalrm Sys.Signal_ignore

let set =
  let raise n = if n = -2 then raise Timeout in
  fun () ->
    Sys.set_signal Sys.sigalrm (Sys.Signal_handle raise);
    let _ : Unix.interval_timer_status =
      ( Unix.setitimer Unix.ITIMER_REAL
          { Unix.it_interval = 0.; Unix.it_value = Param.max_time_execution }
        : Unix.interval_timer_status )
    in
    ()

let timeout_call_run (run : unit -> (unit, Owi.Result.err) Result.t) :
  (unit, [> `Timeout ]) Result.t =
  try
    Fun.protect ~finally:unset (fun () ->
      set ();
      try run () with Timeout -> Error `Timeout )
  with Timeout -> Error `Timeout

module Owi_regular : INTERPRET = struct
  let parse_and_run modul =
    let* simplified = Owi.Compile.Text.until_binary ~unsafe:false modul in
    let* () = Owi.Binary_validate.modul simplified in
    let* regular, link_state =
      Owi.Link.Binary.modul ~name:None Owi.Link.State.empty simplified
    in
    timeout_call_run (fun () ->
      Owi.Interpret.Concrete.modul link_state regular ~timeout:None
        ~timeout_instr:None )

  let name = "owi_concrete"
end

module Reference : INTERPRET = struct
  let parse_and_run modul =
    let* tmp_file = Bos.OS.Dir.tmp "owi_fuzzer_official%s.wast" in
    let* () = Bos.OS.File.writef tmp_file "%a" Owi.Text.Module.pp modul in

    let* cmd =
      Bos.OS.Cmd.resolve
        Bos.Cmd.(
          v "timeout"
          % Fmt.str "%fs" Param.max_time_execution
          % "wasm" % p tmp_file )
    in
    let* status = Bos.OS.Cmd.run_status cmd in
    match status with
    | `Signaled n -> Fmt.error_msg "timeout signaled %d" n
    | `Exited 0 -> Ok ()
    | `Exited 42 ->
      (* TODO: fix this *)
      Error `Out_of_bounds_memory_access
    | `Exited 124 -> Error `Timeout
    | `Exited n -> Fmt.error_msg "error %d" n
  (* TODO: https://github.com/OCamlPro/owi/pull/28#discussion_r1212866678 *)

  let name = "reference"
end

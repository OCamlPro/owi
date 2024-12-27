open Owi
open Syntax

exception Timeout

module type INTERPRET = sig
  type t

  val of_symbolic : Text.modul -> t

  val run : t -> unit Result.t

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

let timeout_call_run (run : unit -> unit Result.t) : 'a Result.t =
  try
    Fun.protect ~finally:unset (fun () ->
      set ();
      try run () with Timeout -> Error `Timeout )
  with Timeout -> Error `Timeout

module Owi_unoptimized : INTERPRET = struct
  type t = Text.modul

  let of_symbolic = Fun.id

  let run modul =
    let* simplified =
      Compile.Text.until_binary ~unsafe:false ~rac:false ~srac:false modul
    in
    let* () = Binary_validate.modul simplified in
    let* regular, link_state =
      Link.modul Link.empty_state ~name:None simplified
    in
    timeout_call_run (fun () ->
      Interpret.Concrete.modul link_state.envs regular )

  let name = "owi"
end

module Owi_optimized : INTERPRET = struct
  type t = Text.modul

  let of_symbolic = Fun.id

  let run modul =
    let* simplified =
      Compile.Text.until_binary ~unsafe:false ~rac:false ~srac:false modul
    in
    let* () = Binary_validate.modul simplified in
    let simplified = Optimize.modul simplified in
    let* regular, link_state =
      Link.modul Link.empty_state ~name:None simplified
    in
    timeout_call_run (fun () ->
      Interpret.Concrete.modul link_state.envs regular )

  let name = "owi+optimize"
end

module Owi_symbolic : INTERPRET = struct
  type t = Text.modul

  let of_symbolic = Fun.id

  let dummy_workers_count = 42

  let run modul : unit Result.t =
    let* simplified =
      Compile.Text.until_binary ~unsafe:false ~rac:false ~srac:false modul
    in
    let* () = Binary_validate.modul simplified in
    let* regular, link_state =
      Link.modul Link.empty_state ~name:None simplified
    in
    let regular = Symbolic.convert_module_to_run_minimalist regular in
    timeout_call_run (fun () ->
      let c = Interpret.SymbolicM.modul link_state.envs regular in
      let init_thread = Thread_with_memory.init () in
      let res, _ =
        Symbolic_choice_minimalist.run ~workers:dummy_workers_count
          Smtml.Solver_type.Z3_solver c init_thread
      in
      match res with
      | Ok res -> res
      | Error (Trap t) -> Error (`Trap t)
      | Error Assert_fail -> Error `Assert_failure )

  let name = "owi_symbolic"
end

module Reference : INTERPRET = struct
  type t = string

  let of_symbolic modul = Fmt.str "%a" Text.pp_modul modul

  let run modul : unit Result.t =
    let* tmp_file = Bos.OS.File.tmp "owi_fuzzer_official%s.wat" in
    let* () = Bos.OS.File.writef tmp_file "%s@\n" modul in
    let* status =
      Bos.OS.Cmd.run_status
        Bos.Cmd.(
          v "timeout" % Fmt.str "%fs" Param.max_time_execution % p tmp_file )
    in
    match status with
    | `Signaled n ->
      Fmt.failwith "error, timeout command was signaled with OCaml signal %d@\n"
        n
    | `Exited 0 -> Ok ()
    | `Exited 42 ->
      (* TODO: fix this *)
      Error (`Trap Trap.Out_of_bounds_memory_access)
    | `Exited 124 -> Error `Timeout
    | `Exited n -> Fmt.error_msg "error %d" n
  (* TODO: https://github.com/OCamlPro/owi/pull/28#discussion_r1212866678 *)

  let name = "reference"
end

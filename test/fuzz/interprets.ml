open Owi
open Syntax

exception Timeout

module type INTERPRET = sig
  val parse_and_run : Text.modul -> unit Result.t

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
  let parse_and_run modul =
    let* simplified =
      Compile.Text.until_binary ~unsafe:false ~rac:false ~srac:false modul
    in
    let* () = Binary_validate.modul simplified in
    let* regular, link_state =
      Link.modul Link.empty_state ~name:None simplified
    in
    timeout_call_run (fun () ->
      Interpret.Concrete.modul link_state.envs regular ~timeout:None
        ~timeout_instr:None )

  let name = "owi_concrete"
end

module Owi_optimized : INTERPRET = struct
  let parse_and_run modul =
    let* simplified =
      Compile.Text.until_binary ~unsafe:false ~rac:false ~srac:false modul
    in
    let* () = Binary_validate.modul simplified in
    let simplified = Optimize.modul simplified in
    let* regular, link_state =
      Link.modul Link.empty_state ~name:None simplified
    in
    timeout_call_run (fun () ->
      Interpret.Concrete.modul link_state.envs regular ~timeout:None
        ~timeout_instr:None )

  let name = "owi_concrete_optimized"
end

module Owi_minimalist_symbolic : INTERPRET = struct
  let dummy_workers_count = 42

  let parse_and_run modul : unit Result.t =
    let* simplified =
      Compile.Text.until_binary ~unsafe:false ~rac:false ~srac:false modul
    in
    let* () = Binary_validate.modul simplified in
    let* regular, link_state =
      Link.modul Link.empty_state ~name:None simplified
    in
    let regular = Minimalist_symbolic.convert_module_to_run regular in
    timeout_call_run (fun () ->
      let c =
        Interpret.Minimalist_symbolic.modul link_state.envs regular
          ~timeout:None ~timeout_instr:None
      in
      let init_thread = Thread_with_memory.init () in
      let res, _ =
        Minimalist_symbolic_choice.run ~workers:dummy_workers_count
          Smtml.Solver_type.Z3_solver c init_thread
      in
      match res with
      | Ok res -> Ok res
      | Error (Trap t) -> Error t
      | Error Assert_fail -> Error `Assert_failure )

  let name = "owi_minimalist_symbolic"
end

module Reference : INTERPRET = struct
  let parse_and_run modul : unit Result.t =
    let* tmp_file = Bos.OS.Dir.tmp "owi_fuzzer_official%s.wast" in
    let* () = Bos.OS.File.writef tmp_file "%a" Text.pp_modul modul in

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

module Owi_full_symbolic (Symbolizer : sig
  val symbolize : Text.modul -> Text.modul
end) : INTERPRET = struct
  let name = "owi_full_symbolic"

  let parse_and_run modul : unit Result.t =
    let modul = Symbolizer.symbolize modul in
    let* simplified =
      Compile.Text.until_binary ~rac:false ~srac:false ~unsafe:false modul
    in
    let* () = Binary_validate.modul simplified in
    let* regular, link_state =
      Link.modul Link.empty_state ~name:None simplified
    in
    let regular = Symbolic.convert_module_to_run regular in
    timeout_call_run (fun () ->
      let c =
        Interpret.Symbolic.modul link_state.envs regular ~timeout:None
          ~timeout_instr:None
      in
      let init_thread = Thread_with_memory.init () in
      let res_acc = ref [] in
      let res_acc_mutex = Mutex.create () in
      let jhs =
        Symbolic_choice_with_memory.run ~workers:1 Smtml.Solver_type.Z3_solver c
          init_thread
          ~callback:(fun (res, _) ->
            Mutex.protect res_acc_mutex (fun () -> res_acc := res :: !res_acc) )
          ~callback_init:(fun () -> ())
          ~callback_end:(fun () -> ())
      in
      Array.iter (fun jh -> Domain.join jh) jhs;
      match !res_acc with
      | [ v ] -> begin
        match v with
        | EVal r -> Ok r
        | ETrap (t, _mdl, _labels, _breadcrumbs, _symbol_scopes) -> Error t
        | EAssert (_expr, _mdl, _labels, _breadcrumbs, _symbol_scopes) ->
          Error `Assert_failure
      end
      | _ -> Fmt.failwith "Unexpected multiple results." )
end

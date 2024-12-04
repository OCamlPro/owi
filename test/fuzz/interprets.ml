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
    ignore
    @@ ( Unix.setitimer Unix.ITIMER_REAL
           { Unix.it_interval = 0.; Unix.it_value = Param.max_time_execution }
         : Unix.interval_timer_status )

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
          Smtml.Solver_dispatcher.Z3_solver c init_thread
      in
      match res with
      | Ok res -> res
      | Error (Trap t) -> Error (`Trap t)
      | Error Assert_fail -> Error `Assert_failure )

  let name = "owi_symbolic"
end

module Reference : INTERPRET = struct
  type t = string

  let of_symbolic modul = Format.asprintf "%a" Text.pp_modul modul

  let run modul : unit Result.t =
    let prefix = "owi_fuzzer_official" in
    let suffix = ".wast" in
    let tmp_file = Filename.temp_file prefix suffix in
    let chan = open_out tmp_file in
    let fmt = Stdlib.Format.formatter_of_out_channel chan in
    Fmt.pf fmt "%s@\n" modul;
    close_out chan;
    let n =
      Format.kasprintf Sys.command "timeout %fs wasm %s"
        Param.max_time_execution tmp_file
    in
    match n with
    | 0 -> Ok ()
    | 42 ->
      (* TODO: fix this *)
      Error (`Trap Trap.Out_of_bounds_memory_access)
    | 124 -> Error `Timeout
    | n -> Error (`Msg (Format.sprintf "error %d" n))
  (* TODO: https://github.com/OCamlPro/owi/pull/28#discussion_r1212866678 *)

  let name = "reference"
end

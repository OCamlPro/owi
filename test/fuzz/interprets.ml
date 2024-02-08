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

let timeout_call_run (run : unit -> 'a Result.t) : 'a Result.t =
  try
    Fun.protect ~finally:unset (fun () ->
        set ();
        try run () with Timeout -> Error "timeout" )
  with Timeout -> Error "timeout"

module Owi_unoptimized : INTERPRET = struct
  type t = Text.modul

  let of_symbolic = Fun.id

  let run modul =
    let* simplified = Compile.until_simplify ~unsafe:false modul in
    let* () = Typecheck.modul simplified in
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
    let* simplified = Compile.until_simplify ~unsafe:false modul in
    let* () = Typecheck.modul simplified in
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

  let run modul =
    let* simplified = Compile.until_simplify ~unsafe:false modul in
    let* () = Typecheck.modul simplified in
    let* regular, link_state =
      Link.modul Link.empty_state ~name:None simplified
    in
    let regular = Symbolic.convert_module_to_run_minimalist regular in
    timeout_call_run (fun () ->
        let c = Interpret.SymbolicM.modul link_state.envs regular in
        let init_thread : Thread.t = Thread.create () in
        let res, _ = Symbolic_choice.Minimalist.run_minimalist c init_thread in
        match res with
        | Ok res -> res
        | Error (Trap _t) -> Result.error "symbolic trap"
        | Error Assert_fail -> Result.error "symbolic assert_fail" )

  let name = "owi_symbolic"
end

module Reference : INTERPRET = struct
  type t = string

  let of_symbolic modul = Format.asprintf "%a" Text.pp_modul modul

  let run modul =
    let prefix = "owi_fuzzer_official" in
    let suffix = ".wast" in
    let tmp_file = Filename.temp_file prefix suffix in
    let chan = open_out tmp_file in
    let fmt = Stdlib.Format.formatter_of_out_channel chan in
    Format.pp_string fmt modul;
    close_out chan;
    let n =
      Format.kasprintf Sys.command "timeout %fs wasm %s"
        Param.max_time_execution tmp_file
    in
    match n with
    | 0 -> Ok ()
    | 42 -> Error "trap"
    | 124 -> Error "timeout"
    | n -> Format.kasprintf failwith "error %d" n
  (* TODO: https://github.com/OCamlPro/owi/pull/28#discussion_r1212866678 *)

  let name = "reference"
end

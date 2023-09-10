open Owi

exception Timeout

module type INTERPRET = sig
  type t

  val of_symbolic : Symbolic.modul -> t

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
  type t = Symbolic.modul

  let of_symbolic = Fun.id

  let run modul =
    match Compile.until_simplify modul with
    | Error e -> failwith e
    | Ok simplified -> (
      match Typecheck.modul simplified with
      | Error e -> failwith e
      | Ok () -> (
        match Link.modul Link.empty_state ~name:None simplified with
        | Error e -> failwith e
        | Ok (regular, _link_state) ->
          timeout_call_run (fun () -> Interpret.I.modul regular) ) )

  let name = "owi"
end

module Owi_optimized : INTERPRET = struct
  type t = Symbolic.modul

  let of_symbolic = Fun.id

  let run modul =
    match Compile.until_simplify modul with
    | Error e -> failwith e
    | Ok simplified -> (
      match Typecheck.modul simplified with
      | Error e -> failwith e
      | Ok () -> (
        let simplified = Optimize.modul simplified in
        match Link.modul Link.empty_state ~name:None simplified with
        | Error e -> failwith e
        | Ok (regular, _link_state) ->
          timeout_call_run (fun () -> Interpret.I.modul regular) ) )

  let name = "owi+optimize"
end

module Reference : INTERPRET = struct
  type t = string

  let of_symbolic modul = Format.asprintf "%a" Symbolic.Pp.modul modul

  let run modul =
    let prefix = "owi_fuzzer_official" in
    let suffix = ".wast" in
    let tmp_file = Filename.temp_file prefix suffix in
    let chan = open_out tmp_file in
    let fmt = Format.formatter_of_out_channel chan in
    Format.pp_print_string fmt modul;
    close_out chan;
    let n =
      Sys.command
      @@ Format.sprintf "timeout %fs wasm %s" Param.max_time_execution tmp_file
    in
    match n with
    | 0 -> Ok ()
    | 42 -> Error "trap"
    | 124 -> Error "timeout"
    | n -> failwith (Format.sprintf "error %d" n)
  (* TODO: https://github.com/OCamlPro/owi/pull/28#discussion_r1212866678 *)

  let name = "reference"
end

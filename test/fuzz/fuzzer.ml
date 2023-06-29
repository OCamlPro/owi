exception Timeout

let () = Random.self_init ()

let timeout_count = ref 0

let global_count = ref 0

let reset () = Sys.set_signal Sys.sigalrm Sys.Signal_ignore

let timeout_call_run (run : unit -> 'a) : 'a option =
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun n -> if n = -2 then raise Timeout));
  let (_ : Unix.interval_timer_status) =
    Unix.setitimer Unix.ITIMER_REAL
      { Unix.it_interval = 0.; Unix.it_value = Param.max_time_execution }
  in
  let res = try Some (run ()) with Timeout -> None in
  reset ();
  res

let compare (module I1 : Interprets.INTERPRET)
  (module I2 : Interprets.INTERPRET) m =
  Format.eprintf "comparing %s and %s@\n    @[<v>" I1.name I2.name;
  Format.eprintf "running %s@\n" I1.name;
  Format.pp_print_flush Format.err_formatter ();
  let r1 =
    let m = I1.of_symbolic m in
    timeout_call_run (fun () -> I1.run m)
  in
  Format.eprintf "running %s@\n" I2.name;
  let r2 =
    let m = I2.of_symbolic m in
    timeout_call_run (fun () -> I2.run m)
  in
  Format.eprintf "@]";
  match (r1, r2) with
  | None, None ->
    incr timeout_count;
    true
  | None, Some _ ->
    Format.eprintf "timeout for `%s` but not for `%s`" I1.name I2.name;
    false
  | Some _, None ->
    Format.eprintf "timeout for `%s` but not for `%s`" I2.name I1.name;
    false
  | Some r1, Some r2 -> (
    match (r1, r2) with
    | Ok (), Ok () -> true
    | Error msg1, Error msg2 ->
      msg1 = msg2
      ||
      ( Format.eprintf "`%s` gave error `%s` but `%s` gave error `%s`" I1.name
          msg1 I2.name msg2;
        false )
    | Ok (), Error msg ->
      Format.eprintf "`%s` was OK but `%s` gave error `%s`" I1.name I2.name msg;
      false
    | Error msg, Ok () ->
      Format.eprintf "`%s` was OK but `%s` gave error `%s`" I2.name I1.name msg;
      false )

let check_optimized m =
  let open Interprets in
  let result1 = compare (module Owi_unoptimized) (module Owi_optimized) m in
  let result2 =
    if Param.reference_fuzzing then
      compare (module Owi_unoptimized) (module Reference) m
    else true
  in
  result1 && result2

let gen = Crowbar.with_printer Owi.Symbolic.Pp.modul Gen.modul

let () =
  Crowbar.add_test ~name:"fuzzing" [ gen ] (fun m ->
    incr global_count;
    Format.eprintf "test module %d [got %d timeouts...]@\n  @[<v>" !global_count
      !timeout_count;
    Format.pp_print_flush Format.err_formatter ();
    Crowbar.check (check_optimized m);
    Format.eprintf "@]" )

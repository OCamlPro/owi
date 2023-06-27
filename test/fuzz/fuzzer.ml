exception Timeout

let () = Random.self_init ()

let timeout_count = ref 0

let timeout_call_run (run : unit -> 'a ) =
  let () = Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> raise Timeout)) in
  let (_ : Unix.interval_timer_status) =
    Unix.setitimer Unix.ITIMER_REAL { Unix.it_interval = 0.; Unix.it_value = Param.max_time_execution }
  in
  try Some (run ()) with
  | Timeout -> None

let check_optimized m =
  let open Interprets in
  let result_unoptimized =
    let open Owi_unoptimized in
    let m = of_symbolic m in
    timeout_call_run (fun () -> run m)
  in
  let result_optimized =
    let open Owi_optimized in
    let m = of_symbolic m in
    timeout_call_run (fun () -> run m)
  in
  let result_reference () =
    let open Reference in
    let m = of_symbolic m in
    timeout_call_run (fun () -> run m)
  in
  let result1 =
    match (result_unoptimized, result_optimized) with
    | None, None -> incr timeout_count; print_endline "Timeout!"; true
    | Some Ok (), Some Ok () -> true
    | Some Error msg1, Some Error msg2 when msg1 = msg2 -> true
    | Some Error msg1, Some Error msg2 ->
      Format.ksprintf failwith
        "unoptimized module and optimized module interpretations throw \
         different errors: %s / opti: %s"
        msg1 msg2
    | Some Error msg, Some Ok () ->
      Format.ksprintf failwith
        "only unoptimized module interpretation throws an error: %s" msg
    | Some Ok (), Some Error msg ->
      Format.ksprintf failwith
        "only optimized module interpretation throws an error: %s" msg
    | None, Some _ -> 
      Format.ksprintf failwith
        "only unoptimized module interpretation throws a timeout error"
    | Some _, None ->
      Format.ksprintf failwith
        "only optimized module interpretation throws a timeout error"
  in
  let result2 =
    (not Param.reference_fuzzing)
    ||
    match (result_unoptimized, result_reference ()) with
    | None, None -> true
    | Some Ok (), Some Ok () -> true
    | Some Error _msg1, Some Error _msg2 ->
      (* TODO: parse the output of result_reference to check that the error are the same*)
      true
    | Some Error msg, Some Ok () ->
      Format.ksprintf failwith
        "only unoptimized module interpretation throws an error: %s" msg
    | Some Ok (), Some Error msg ->
      Format.ksprintf failwith
        "only reference module interpretation throws an error: %s" msg
    | None, Some _ -> 
      Format.ksprintf failwith
        "only unoptimized module interpretation throws a timeout error"
    | Some _, None ->
      Format.ksprintf failwith
        "only optimized module interpretation throws a timeout error"
  in
  result1 && result2

let is_optimized m = Crowbar.check (check_optimized m)

let () =
  print_newline ();
  print_endline "Owi fuzzing ...";
  let count = ref 0 in
  let fmt = Format.err_formatter in
  Crowbar.add_test ~name:"Optimize fuzzing" [ Gen.modul ] (fun m ->
    incr count;
    Format.fprintf fmt
      "Generating new module (%d/5000 including %d timeouts)...@\n"
      !count !timeout_count;
    if Param.debug then Format.fprintf fmt "%a@\n" Owi.Symbolic.Pp.modul m;
    Format.pp_print_flush fmt ();
    is_optimized m );

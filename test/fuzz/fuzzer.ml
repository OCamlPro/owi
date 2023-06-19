let () = Random.self_init ()

let check_optimized m =
  let open Interprets in
  let result_unoptimized =
    let open Owi_unoptimized in
    of_symbolic m |> run
  in
  let result_optimized =
    let open Owi_optimized in
    of_symbolic m |> run
  in
  let result_reference () =
    let open Reference in
    of_symbolic m |> run
  in
  let result1 =
    match (result_unoptimized, result_optimized) with
    | Ok (), Ok () -> true
    | Error msg1, Error msg2 when msg1 = msg2 -> true
    | Error msg1, Error msg2 ->
      Format.ksprintf failwith
        "unoptimized module and optimized module interpretations throw \
         different errors: %s / opti: %s"
        msg1 msg2
    | Error msg, Ok () ->
      Format.ksprintf failwith
        "only unoptimized module interpretation throws an error: %s" msg
    | Ok (), Error msg ->
      Format.ksprintf failwith
        "only optimized module interpretation throws an error: %s" msg
  in
  let result2 =
    not Param.reference_fuzzing ||
    match (result_unoptimized, result_reference ()) with
    | Ok (), Ok () -> true
    | Error _msg1, Error _msg2 ->
      (* TODO: parse the output of result_reference to check that the error are the same*)
      true
    | Error msg, Ok () ->
      Format.ksprintf failwith
        "only unoptimized module interpretation throws an error: %s" msg
    | Ok (), Error msg ->
      Format.ksprintf failwith
        "only reference module interpretation throws an error: %s" msg
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
    Format.fprintf fmt "Generating new module (%d/5000)...@\n" !count;
    if Param.debug then Format.fprintf fmt "%a@\n" Owi.Types.Symbolic.Pp.modul m;
    Format.pp_print_flush fmt ();
    is_optimized m )

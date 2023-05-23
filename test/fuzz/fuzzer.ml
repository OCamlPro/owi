open Owi

let () = Random.self_init ()

let check_optimized m =
  let simplified =
    match Compile.until_simplify m with Error e -> failwith e | Ok m -> m
  in
  (match Typecheck.modul simplified with
  | Ok () -> ()
  | Error msg -> failwith msg);
  let regular, _link_state =
    match Link.modul Link.empty_state ~name:None simplified with
    | Error e -> failwith e
    | Ok m -> m
  in
  let optimized = Optimize.modul simplified in 
  (match Typecheck.modul optimized with
  | Ok () -> ()
  | Error msg -> failwith msg);
  let optimized, _link_state =
    match
      simplified |> Link.modul Link.empty_state ~name:None
    with
    | Error e -> failwith e
    | Ok m -> m
  in
  let result = Interpret.modul regular in
  let result_optimized = Interpret.modul optimized in
  match (result, result_optimized) with
  | Ok (), Ok () -> true
  | Error msg1, Error msg2 when msg1 = msg2 -> true
  | Error msg1, Error msg2 ->
    Format.ksprintf failwith
      "module and optimized module interpretations throw different errors: %s \
       / opti: %s"
      msg1 msg2
  | Error msg, Ok () ->
    Format.ksprintf failwith "only module interpretation throws an error: %s"
      msg
  | Ok (), Error msg ->
    Format.ksprintf failwith
      "only optimized module interpretation throws an error: %s" msg

let is_optimized m = Crowbar.check (check_optimized m)

let () =
  print_newline ();
  print_endline "Owi fuzzing ...";
  let count = ref 0 in
  let fmt = Format.err_formatter in
  Crowbar.add_test ~name:"Optimize fuzzing" [ Gen.modul ] (fun m ->
    incr count;
    Format.fprintf fmt "Generating new module (%d/5000)...@\n" !count;
    Env.reset ();
    Format.fprintf fmt "%a@\n" Owi.Types.Symbolic.Pp.modul m;
    Format.pp_print_flush fmt ();
    is_optimized m )

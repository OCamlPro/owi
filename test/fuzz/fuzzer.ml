let () = Random.self_init ()

let timeout_count = ref 0

let global_count = ref 0

let compare (module I1 : Interprets.INTERPRET)
  (module I2 : Interprets.INTERPRET) m =
  if Param.debug then begin
    Format.eprintf "comparing %s and %s@\n    @[<v>" I1.name I2.name;
    Format.eprintf "running %s@\n" I1.name;
    Format.pp_print_flush Format.err_formatter ()
  end;
  let r1 =
    let m = I1.of_symbolic m in
    I1.run m
  in
  if Param.debug then begin
    Format.eprintf "running %s@\n" I2.name
  end;
  let r2 =
    let m = I2.of_symbolic m in
    I2.run m
  in
  Format.eprintf "@]";
  match (r1, r2) with
  | Ok (), Ok () -> true
  | Error "timeout", Error "timeout" ->
    incr timeout_count;
    true
  | Error "timeout", Ok () ->
    Param.allow_partial_timeout
    ||
    ( Format.eprintf "timeout for `%s` but not for `%s`" I1.name I2.name;
      false )
  | Ok (), Error "timeout" ->
    Param.allow_partial_timeout
    ||
    ( Format.eprintf "timeout for `%s` but not for `%s`" I2.name I1.name;
      false )
  | Error "timeout", Error msg ->
    Param.allow_partial_timeout
    ||
    ( Format.eprintf "timeout for `%s` but error `%s` for `%s`" I1.name msg
        I2.name;
      false )
  | Error msg, Error "timeout" ->
    Param.allow_partial_timeout
    ||
    ( Format.eprintf "timeout for `%s` but error `%s` for `%s`" I2.name msg
        I1.name;
      false )
  | Error msg1, Error msg2 ->
    true (* TODO: fixme *) || msg1 = msg2
    ||
    ( Format.eprintf "`%s` gave error `%s` but `%s` gave error `%s`" I1.name
        msg1 I2.name msg2;
      false )
  | Ok (), Error msg ->
    Format.eprintf "`%s` was OK but `%s` gave error `%s`" I1.name I2.name msg;
    false
  | Error msg, Ok () ->
    Format.eprintf "`%s` was OK but `%s` gave error `%s`" I2.name I1.name msg;
    false

let check_optimized m =
  let open Interprets in
  let result1 =
    if Param.optimize_fuzzing then
      compare (module Owi_unoptimized) (module Owi_optimized) m
    else true
  in
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
    Format.eprintf "test module %d [got %d timeouts...]@\n@[<v>" !global_count
      !timeout_count;
    Format.pp_print_flush Format.err_formatter ();
    Crowbar.check (check_optimized m);
    Format.eprintf "@]" )

let () = Random.self_init ()

let timeout_count = ref 0

let global_count = ref 0

let write_module (fpath : Fpath.t) m = 
  match Bos.OS.File.writef fpath "%a@." Owi.Text.pp_modul m with
  | Ok () -> Ok ()
  | Error (`Msg err) -> Error (`Msg (Fmt.str "Failed to write module to %a: %s" Fpath.pp fpath err))

let compare (module I1 : Interprets.INTERPRET)
  (module I2 : Interprets.INTERPRET) m =
  if Param.debug then begin
    Fmt.epr "comparing %s and %s@\n    @[<v>" I1.name I2.name;
    Fmt.epr "running %s@\n" I1.name;
    Fmt.flush Fmt.stderr ()
  end;
  let r1 =
    let m = I1.of_symbolic m in
    I1.run m
  in
  if Param.debug then begin
    Fmt.epr "running %s@\n" I2.name
  end;
  let r2 =
    let m = I2.of_symbolic m in
    I2.run m
  in
  Fmt.epr "@]";
  match (r1, r2) with
  | Ok (), Ok () -> true
  | Error `Timeout, Error `Timeout ->
    incr timeout_count;
    true
  | Error `Timeout, Ok () ->
    Param.allow_partial_timeout
    ||
    ( Fmt.epr "timeout for `%s` but not for `%s`" I1.name I2.name;
      false )
  | Ok (), Error `Timeout ->
    Param.allow_partial_timeout
    ||
    ( Fmt.epr "timeout for `%s` but not for `%s`" I2.name I1.name;
      false )
  | Error `Timeout, Error msg ->
    let msg = Owi.Result.err_to_string msg in
    Param.allow_partial_timeout
    ||
    ( Fmt.epr "timeout for `%s` but error `%s` for `%s`" I1.name msg I2.name;
      false )
  | Error msg, Error `Timeout ->
    let msg = Owi.Result.err_to_string msg in
    Param.allow_partial_timeout
    ||
    ( Fmt.epr "timeout for `%s` but error `%s` for `%s`" I2.name msg I1.name;
      false )
  | Error msg1, Error msg2 ->
    let msg1 = Owi.Result.err_to_string msg1 in
    let msg2 = Owi.Result.err_to_string msg2 in
    true (* TODO: fixme *) || msg1 = msg2
    ||
    ( Fmt.epr "`%s` gave error `%s` but `%s` gave error `%s`" I1.name msg1
        I2.name msg2;
      false )
  | Ok (), Error msg ->
    let msg = Owi.Result.err_to_string msg in
    Fmt.epr "`%s` was OK but `%s` gave error `%s`" I1.name I2.name msg;
    false
  | Error msg, Ok () ->
    let msg = Owi.Result.err_to_string msg in
    Fmt.epr "`%s` was OK but `%s` gave error `%s`" I2.name I1.name msg;
    false

let check (module I1 : Interprets.INTERPRET) (module I2 : Interprets.INTERPRET) m =
  if Param.save_modules then begin
    let outdir = Fpath.v Param.output_dir in
    let* () = Bos.OS.Dir.create ~mode:0o755 outdir in
    let filename = Fpath.(v Param.output_dir / Fmt.str "gen_do_module_%d.wat" !global_count) in
    let* () = write_module filename m in
    if Param.debug then
      Fmt.epr "Saved module to %a@\n" Fpath.pp filename;
    Ok ()
  end else
    Ok ();
  
  compare (module I1) (module I2) m

let add_test name gen (module I1 : Interprets.INTERPRET)
  (module I2 : Interprets.INTERPRET) =
  Crowbar.add_test ~name [ gen ] (fun m ->
    incr global_count;
    if Param.debug then Fmt.epr "%a@\n" Owi.Text.pp_modul m;
    Fmt.epr "test module %d [got %d timeouts...]@\n@[<v>" !global_count
      !timeout_count;
    Fmt.flush Fmt.stderr ();
    Crowbar.check (check (module I1) (module I2) m);
    Fmt.epr "@]" )

let gen (conf : Env.conf) =
  Crowbar.with_printer Owi.Text.pp_modul (Gen.modul conf)

let () =
  let open Interprets in
  if Param.optimize_fuzzing then
    add_test "optimize_fuzzing" (gen Env.Concrete)
      (module Owi_unoptimized)
      (module Owi_optimized);
  if Param.reference_fuzzing then
    add_test "reference_fuzzing" (gen Env.Concrete)
      (module Owi_unoptimized)
      (module Reference);
  if Param.symbolic_fuzzing then
    add_test "symbolic_fuzzing" (gen Env.Symbolic)
      (module Owi_unoptimized)
      (module Owi_symbolic)

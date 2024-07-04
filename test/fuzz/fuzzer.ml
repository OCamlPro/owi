open Owi

let () = Random.self_init ()

let timeout_count = ref 0

let global_count = ref 0

let compare (module I1 : Interprets.INTERPRET)
  (module I2 : Interprets.INTERPRET) m =
  if Param.debug then begin
    Format.pp_err "comparing %s and %s@\n    @[<v>" I1.name I2.name;
    Format.pp_err "running %s@\n" I1.name;
    Format.pp_flush Stdlib.Format.err_formatter ()
  end;
  let r1 = I1.parse_and_run m in
  if Param.debug then begin
    Format.pp_err "running %s@\n" I2.name
  end;
  let r2 = I2.parse_and_run m in
  Format.pp_err "@]";
  match (r1, r2) with
  | Ok (), Ok () -> true
  | Error `Timeout, Error `Timeout ->
    incr timeout_count;
    true
  | Error `Timeout, Ok () ->
    Param.allow_partial_timeout
    ||
    ( Format.pp_err "timeout for `%s` but not for `%s`" I1.name I2.name;
      false )
  | Ok (), Error `Timeout ->
    Param.allow_partial_timeout
    ||
    ( Format.pp_err "timeout for `%s` but not for `%s`" I2.name I1.name;
      false )
  | Error `Timeout, Error msg ->
    let msg = Owi.Result.err_to_string msg in
    Param.allow_partial_timeout
    ||
    ( Format.pp_err "timeout for `%s` but error `%s` for `%s`" I1.name msg
        I2.name;
      false )
  | Error msg, Error `Timeout ->
    let msg = Owi.Result.err_to_string msg in
    Param.allow_partial_timeout
    ||
    ( Format.pp_err "timeout for `%s` but error `%s` for `%s`" I2.name msg
        I1.name;
      false )
  | Error msg1, Error msg2 ->
    let msg1 = Owi.Result.err_to_string msg1 in
    let msg2 = Owi.Result.err_to_string msg2 in
    true (* TODO: fixme *) || msg1 = msg2
    ||
    ( Format.pp_err "`%s` gave error `%s` but `%s` gave error `%s`" I1.name msg1
        I2.name msg2;
      false )
  | Ok (), Error msg ->
    let msg = Owi.Result.err_to_string msg in
    Format.pp_err "`%s` was OK but `%s` gave error `%s`" I1.name I2.name msg;
    false
  | Error msg, Ok () ->
    let msg = Owi.Result.err_to_string msg in
    Format.pp_err "`%s` was OK but `%s` gave error `%s`" I2.name I1.name msg;
    false

let check (module I1 : Interprets.INTERPRET) (module I2 : Interprets.INTERPRET)
  m =
  compare (module I1) (module I2) m

let add_test name gen (module I1 : Interprets.INTERPRET)
  (module I2 : Interprets.INTERPRET) =
  Crowbar.add_test ~name [ gen ] (fun m ->
      incr global_count;
      if Param.debug then Format.pp_err "%a@\n" Owi.Text.pp_modul m;
      Format.pp_err "test module %d [got %d timeouts...]@\n@[<v>" !global_count
        !timeout_count;
      Format.pp_flush Stdlib.Format.err_formatter ();
      Crowbar.check (check (module I1) (module I2) m);
      Format.pp_err "@]" )

let gen (conf : Env.conf) =
  Crowbar.with_printer Owi.Text.pp_modul (Gen.modul conf)

let prepare_for_sym (modul : Owi.Text.modul) =
  let open Owi.Text in
  let open Types in
  let process_inst inst inst_list =
    match inst with
    | I32_const i -> I32_const i :: Call (Text "constant_symbol") :: inst_list
    | _ -> inst :: inst_list
  in
  let process_func fnc =
    let updated_body = List.fold_right process_inst fnc.body [] in
    { fnc with body = updated_body }
  in
  let process_field = function
    | MFunc fnc -> MFunc (process_func fnc)
    | fld -> fld
  in
  let import_sym_cst_i32 =
    MImport
      { modul = "symbolic"
      ; name = "symbol_i32_constant"
      ; desc =
          Import_func
            ( Some "symbol_i32_constant"
            , Bt_raw (None, ([ (None, Num_type I32) ], [ Num_type I32 ])) )
      }
  in
  let updated_fields =
    import_sym_cst_i32 :: List.map process_field modul.fields
  in
  { modul with fields = updated_fields }

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
      (module Owi_symbolic);
  if Param.full_symbolic_fuzzing then
    add_test "full symbolic_fuzzing" (gen Env.Symbolic)
      (module Owi_unoptimized)
      ( module Owi_symbolic_multicore (struct
        let symbolize = prepare_for_sym
      end) )

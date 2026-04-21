(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let model = ref []

let reset () =
  model := []

let pp_model ppf model =
  Fmt.list ~sep:(fun ppf () -> Fmt.pf ppf " ; ") Concrete_value.pp ppf model

let symbol_i32 () =
  let n = Random.bits32 () in
  let n = Concrete_i32.of_int32 n in
  model := (Concrete_value.I32 n) :: !model;
  Ok n

let assert' n =
  if Prelude.Int32.equal 0l n then Error (`Msg "I found a bug") else Ok ()

let extern_module =
  let open Concrete_extern_func in
  let open Concrete_extern_func.Syntax in
  let functions =
    [ ("i32_symbol", Extern_func (unit ^->. i32, symbol_i32))
    ; ("assert", Extern_func (i32 ^->. unit, assert'))
    ]
  in
  { Extern.Module.functions; func_type = Concrete_extern_func.extern_type }

let rec fuzzing_loop ~rounds f =
  match f () with
  | Ok () ->
    reset ();
    begin match rounds with
    | None -> fuzzing_loop ~rounds f
    | Some 0 -> Ok ()
    | Some n -> fuzzing_loop ~rounds:(Some (pred n)) f
    end
  | Error _ as e ->
      Log.app (fun m -> m "Found a bug with model: %a" pp_model !model);
      e

let cmd ~rounds ~unsafe ~timeout ~timeout_instr ~source_file =
  let link_state =
    Link.State.empty () |> Link.Extern.modul ~name:"owi" extern_module
  in
  let* m, link_state =
    Compile.File.until_link ~unsafe ~name:None link_state source_file
  in
  let module Parameters = struct
    let timeout = timeout

    let timeout_instr = timeout_instr

    let use_ite_for_select = true

    let throw_away_trap = false
  end in
  let module I = Interpret.Concrete (Parameters) in
  let res, run_time =
    Benchmark.with_utime @@ fun () ->
    fuzzing_loop ~rounds (fun () ->
      (* TODO: check if we should regenerate the link state *)
      I.modul link_state m )
  in
  Log.bench (fun m ->
    (* run_time shouldn't be none in bench mode *)
    let run_time = match run_time with None -> assert false | Some t -> t in
    m "Benchmarks:@\n@[<v>interpreter time: %fms@]" (run_time *. 1000.) );
  res

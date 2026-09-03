(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd ~rounds ~seed ~source_file ~timeout ~timeout_instr ~unsafe =
  Init.random_state seed;
  let env = Env.Concrete.empty ~context:() in
  let* env =
    Env.Concrete.link_extern_module ~env ~name:"owi" Fuzz_wasm_ffi.owi
  in
  let* modul = Compile.File.until_validate ~unsafe source_file in
  let module Parameters = struct
    let timeout = timeout

    let timeout_instr = timeout_instr

    let use_ite_for_select = true

    let throw_away_trap = false

    let abstract_invariant = Abstract_invariant.empty ()
  end in
  let module I = Interpret.Concrete (Parameters) in
  let res, run_time =
    Benchmark.with_utime @@ fun () ->
    Fuzz_driver.run ~rounds (fun () ->
      (* TODO: for now we have to regenerate the environment on each round because the concrete environment still is mutable, this should be avoided in the future *)
      let* env = Env.Concrete.link_binary_module ~env ~name:None ~modul in
      let* modul = Env.Concrete.get_last_module ~env in
      let* _env = I.modul ~env ~modul in
      Ok () )
  in
  Log.bench (fun m ->
    (* run_time shouldn't be none in bench mode *)
    let run_time = match run_time with None -> assert false | Some t -> t in
    m "Benchmarks:@\n@[<v>interpreter time: %fms@]" (run_time *. 1000.) );
  res

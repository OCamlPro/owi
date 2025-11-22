(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let cmd ~unsafe ~timeout ~timeout_instr ~source_file =
  let name = None in
  let link_state = Link.State.empty () in
  let* m, link_state =
    Compile.File.until_link ~unsafe ~name link_state source_file
  in
  let res, run_time =
    Benchmark.with_utime @@ fun () ->
    Interpret.Concrete.modul ~timeout ~timeout_instr link_state m
  in
  Log.bench (fun m ->
    (* run_time shouldn't be none in bench mode *)
    let run_time = match run_time with None -> assert false | Some t -> t in
    m "Benchmarks:@\n@[<v>interpreter time: %fms@]" (run_time *. 1000.) );
  res

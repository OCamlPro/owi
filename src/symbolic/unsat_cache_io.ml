(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

open Bos

let save cache file =
  let entries =
    Unsat_cache.fold
      (fun fp _cores acc ->
        let hash = Hash_footprint.hash fp in
        hash :: acc)
      cache []
  in
  let json = `List (List.map (fun h -> `Int h) entries) in
  let json_str = Yojson.Safe.to_string json in
  match OS.File.write file json_str with
  | Ok () -> Ok ()
  | Error (`Msg msg) -> Error msg

let load file =
  let cache = Unsat_cache.create () in
  match OS.File.read file with
  | Error _ -> Ok cache
  | Ok content ->
      match Yojson.Safe.from_string content with
      | `List entries ->
          List.iter (function
            | `Int h ->
                let fp = Hash_footprint.of_hash h in
                Unsat_cache.add cache fp []
            | _ -> ())
            entries;
          Ok cache
      | _ -> Error "Invalid cache file format"
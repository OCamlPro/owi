(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Syntax

let py_location = List.map Fpath.v C_share_site.Sites.pyc

let bin_location = List.map Fpath.v C_share_site.Sites.binc

let lib_location = List.map Fpath.v C_share_site.Sites.libc

let find location file =
  let* l =
    list_map
      (fun dir ->
        let filename = Fpath.append dir file in
        match Bos.OS.File.exists filename with
        | Ok true -> Ok (Some filename)
        | Ok false -> Ok None
        | Error (`Msg msg) -> Error (`Msg msg) )
      location
  in
  Ok (List.find (function None -> false | Some _filename -> true) l)

let libc =
  Option.get @@ Result.get_ok @@ find bin_location (Fpath.v "libc.wasm")

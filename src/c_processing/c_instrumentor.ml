(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let py_module = lazy (Py.Import.import_module "instrumentor")

let import_module () = Lazy.force py_module

let instrument file includes =
  let callable = Py.Module.get (import_module ()) "instrument" in
  let kwargs =
    [ ("file", Py.String.of_string @@ Fpath.to_string file)
    ; ( "includes"
      , Py.List.of_list
        @@ List.map
             (fun path -> Py.String.of_string (Fpath.to_string path))
             includes )
    ]
  in
  let _ : Py.Object.t =
    Py.Callable.to_function_with_keywords callable [||] kwargs
  in
  ()

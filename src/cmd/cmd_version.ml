(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

let string_of_version = function
  | None -> "unknown"
  | Some v -> Build_info.V1.Version.to_string v

let version () = string_of_version @@ Build_info.V1.version ()

let statically_linked_libraries () =
  Build_info.V1.Statically_linked_libraries.to_list ()
  |> List.map (fun l ->
    ( Build_info.V1.Statically_linked_library.name l
    , Build_info.V1.Statically_linked_library.version l |> string_of_version ) )
  |> List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2)

let pp fmt () =
  Fmt.list
    ~sep:(fun fmt () -> Fmt.pf fmt "@\n")
    (fun fmt (name, version) -> Fmt.pf fmt "%s version %s" name version)
    fmt
    (("owi", version ()) :: statically_linked_libraries ());
  Fmt.string fmt "@\n"

let owi_version () = Fmt.str "owi version %s" (version ())

let cmd () = Ok (Log.app (fun m -> m "%a" pp ()))

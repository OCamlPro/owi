(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type ('local, 'imported) t =
  | Local of 'local
  | Imported of 'imported Imported.t

val pp :
     pp_local:'local Fmt.t
  -> pp_imported:'imported Fmt.t
  -> Format.formatter
  -> ('local, 'imported) t
  -> unit

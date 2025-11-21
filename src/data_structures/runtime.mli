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

val map :
     f_local:('local -> 'local2)
  -> f_imported:('imported Imported.t -> 'imported2 Imported.t)
  -> ('local, 'imported) t
  -> ('local2, 'imported2) t

val monadic_map :
     f_local:('local -> 'local2 Result.t)
  -> f_imported:('imported Imported.t -> 'imported2 Imported.t Result.t)
  -> ('local, 'imported) t
  -> ('local2, 'imported2) t Result.t

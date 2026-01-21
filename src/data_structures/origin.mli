(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a imported =
  { modul_name : string
  ; name : string
  ; assigned_name : string option
  ; typ : 'a
  }

type ('local, 'imported) t =
  | Local of 'local
  | Imported of 'imported imported

val imported :
     modul_name:string
  -> name:string
  -> assigned_name:string option
  -> typ:'a
  -> (_, 'a) t

val pp :
     pp_local:'local Fmt.t
  -> pp_imported:'imported Fmt.t
  -> ('local, 'imported) t Fmt.t

val map :
     f_local:('local -> 'local2)
  -> f_imported:('imported -> 'imported2)
  -> ('local, 'imported) t
  -> ('local2, 'imported2) t

val monadic_map :
     f_local:('local -> 'local2 Result.t)
  -> f_imported:('imported -> 'imported2 Result.t)
  -> ('local, 'imported) t
  -> ('local2, 'imported2) t Result.t

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type value

  type extern_func

  type modul

  val empty : t

  val get_last_module : runtime:t -> modul Result.t

  val register_module : runtime:t -> modul:modul -> name:string -> t

  val get_initialization_code : runtime:t -> modul:modul -> Binary.expr

  val link_binary_module :
    runtime:t -> name:string option -> modul:Binary.Module.t -> t Result.t

  (* TODO: the name should be removed and people should call register_module so we get a uniform API compared to link_binary module *)
  val link_extern_module :
    runtime:t -> name:string -> (string * extern_func) list -> t

  val get_global : runtime:t -> int -> value

  val set_global : runtime:t -> int -> value -> t

  val get_func : runtime:t -> int -> Kind.func

  val get_extern_func : runtime:t -> int -> extern_func

  val get_exported_func :
       runtime:t
    -> module_name:string option
    -> func_name:string
    -> Kind.func Result.t

  type context

  val get_context : runtime:t -> context
end

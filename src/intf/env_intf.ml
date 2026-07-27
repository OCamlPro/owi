(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type value

  type extern_func

  type modul

  type memory

  type table

  type elem

  type data

  val empty : t

  val pp : t Fmt.t

  val get_last_module : env:t -> modul Result.t

  val register_module :
    env:t -> name:string -> modid:string option -> t Result.t

  val get_initialization_code : env:t -> modul:modul -> Binary.expr

  val link_binary_module :
    env:t -> name:string option -> modul:Binary.Module.t -> t Result.t

  (* TODO: the name should be removed and people should call register_module so we get a uniform API compared to link_binary module *)
  val link_extern_module :
    env:t -> name:string -> (string * extern_func) list -> t Result.t

  val get_memory : env:t -> int -> memory

  val set_memory : env:t -> int -> memory -> t

  val get_elem : env:t -> int -> elem

  val set_elem : env:t -> int -> elem -> t

  val get_table : env:t -> int -> table

  val set_table : env:t -> int -> table -> t

  val get_data : env:t -> int -> data

  val set_data : env:t -> int -> data -> t

  val get_global : env:t -> int -> value

  val set_global : env:t -> int -> value -> t

  val get_func : env:t -> int -> extern_func Kind.func

  val get_exported_func :
       env:t
    -> module_name:string option
    -> func_name:string
    -> extern_func Kind.func Result.t

  val get_exported_global :
    env:t -> module_name:string option -> global_name:string -> value Result.t

  type context

  val get_context : env:t -> context

  val get_modul_from_modid : env:t -> modid:string -> modul Result.t
end

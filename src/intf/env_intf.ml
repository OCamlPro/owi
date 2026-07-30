(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type t

  type extern_func

  type extern_module

  type data

  type memory

  type global

  type elem

  type table

  type 'a choice

  val get_memory : t -> int -> memory choice

  val get_func : t -> int -> Kind.func

  val get_table : t -> int -> table choice

  val get_elem : t -> int -> elem

  val get_data : t -> int -> data

  val get_global : t -> int -> global choice

  val get_extern_func : t -> int -> extern_func

  val get_init_code : modul:int -> t -> Binary.expr Annotated.t

  val link_extern_module : name:string -> extern_module -> t -> t

  val link_binary_module :
    name:string option -> t -> Binary.Module.t -> (int * t) Result.t

  val get_exported_func :
    t -> module_name:string option -> func_name:string -> Kind.func Result.t

  val get_exported_global :
       t
    -> module_name:string option
    -> global_name:string
    -> Concrete_global.t Result.t

  val empty : unit -> t

  (** give a named to the last linked module in the given link state *)
  val register_last_module : t -> name:string -> id:string option -> t Result.t
end

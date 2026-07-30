(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type M = sig
  type extern_func

  type extern_module = (string * extern_func) list

  val to_func_type : extern_func -> Binary.func_type

  type data

  val data_of_concrete : Concrete_data.t -> data
end

module type T = sig
  type extern_func

  type extern_module

  type data

  type t

  (** the empty link state *)
  val empty : unit -> t

  val get_memory : t -> int -> Concrete_memory.t

  val get_data : t -> int -> data

  val get_func : t -> int -> Kind.func

  val get_table : t -> int -> Concrete_table.t

  val get_elem : t -> int -> Concrete_elem.t

  val get_global : t -> int -> Concrete_global.t

  val get_extern_func : t -> int -> extern_func

  val get_init_code : modul:int -> t -> Binary.expr Annotated.t

  val get_exported_global :
       t
    -> module_name:string option
    -> global_name:string
    -> Concrete_global.t Result.t

  val get_exported_func :
    t -> module_name:string option -> func_name:string -> Kind.func Result.t

  (** give a named to the last linked module in the given link state *)
  val register_last_module : t -> name:string -> id:string option -> t Result.t

  (* Link *)

  (** register an extern module with a given link state, producing a new link
      state *)
  val link_extern_module : name:string -> extern_module -> t -> t

  (* TODO: change this to name:.. -> (state*module) -> (state*module) so that it can be piped easily *)

  (** link a module with a given link state, producing a runnable module and a
      new link state *)
  val link_binary_module :
    name:string option -> t -> Binary.Module.t -> (int * t) Result.t
end

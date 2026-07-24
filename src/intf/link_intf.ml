(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module type T = sig
  type extern_func

  type extern_module

  module State : sig
    type t

    (** the empty link state *)
    val empty : unit -> t

    val get_memory : modul:int -> t -> int -> Concrete_memory.t

    val get_data : modul:int -> t -> int -> Concrete_data.t

    val get_func : modul:int -> t -> int -> Kind.func

    val get_table : modul:int -> t -> int -> Concrete_table.t

    val get_elem : modul:int -> t -> int -> Concrete_elem.t

    val get_global : modul:int -> t -> int -> Concrete_global.t

    val fold_globals :
      modul:int -> (int -> Concrete_global.t -> 'a -> 'a) -> 'a -> t -> 'a

    val get_extern_func : modul:int -> t -> int -> extern_func

    val get_init_code : modul:int -> t -> Binary.expr Annotated.t

    val get_exported_global :
         t
      -> module_name:string option
      -> global_name:string
      -> Concrete_global.t Result.t

    val get_exported_func :
         t
      -> module_name:string option
      -> func_name:string
      -> (Kind.func * int) Result.t

    (** give a named to the last linked module in the given link state *)
    val register_last_module :
      t -> name:string -> id:string option -> t Result.t
  end

  (* Link *)

  module Extern : sig
    (** register an extern module with a given link state, producing a new link
        state *)
    val modul : name:string -> extern_module -> State.t -> State.t
  end

  module Binary : sig
    (* TODO: change this to name:.. -> (state*module) -> (state*module) so that it can be piped easily *)
    (** link a module with a given link state, producing a runnable module and a
        new link state *)
    val modul :
         name:string option
      -> State.t
      -> Binary.Module.t
      -> (int * State.t) Result.t
  end
end

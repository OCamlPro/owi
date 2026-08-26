(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2026 OCamlPro *)
(* Written by the Owi programmers *)

module Make
    (Context : sig
      type t

      val empty : unit -> t
    end)
    (Value : sig
      type t

      val pp : t Fmt.t

      module Ref : sig
        module Extern : sig
          type t
        end

        type i32

        module Array : Array_intf.T

        module Struct : Struct_intf.T

        type 'value t =
          | Extern of Extern.t option
          | Func of int option
          | NullExn
          | NullRef
          | I31 of i32
          | NullI31
          | Array of 'value Array.t
          | Struct of 'value Struct.t
          | ExternAsAny of Extern.t option
          | AnyAsExtern of 'value t
      end
    end)
    (Constexpr_eval :
      Constexpr_eval_intf.T
        with type value := Value.t
         and type reference := Value.t Value.Ref.t
         and type context := Context.t)
    (Memory : sig
      type t

      val get_limits : t -> Binary.Mem.Type.limits

      val init : Binary.Mem.Type.limits -> t
    end)
    (Table : Table_intf.T with type reference := Value.t Value.Ref.t)
    (Elem : Elem_intf.T with type reference := Value.t Value.Ref.t)
    (Extern_func : sig
      type t

      val to_func_type : t -> Binary.func_type
    end)
    (Data : Data_intf.T) :
  Env_intf.T
    with type extern_func := Extern_func.t
     and type value := Value.t
     and type elem := Elem.t
     and type data := Data.t
     and type table := Table.t
     and type memory := Memory.t
     and type context = Context.t = struct
  include Env0

  let empty = Env0.empty ~empty_context:Context.empty

  include
    Env_linker.Make (Context) (Value) (Constexpr_eval) (Memory) (Table) (Elem)
      (Extern_func)
      (Data)
end

module Dummmy_context = struct
  type t = unit

  let empty () = ()
end

module Concrete =
  Make (Dummmy_context) (Concrete_value) (Constexpr_eval.Concrete)
    (Concrete_memory)
    (Concrete_table)
    (Concrete_elem)
    (Concrete_extern.Func)
    (Concrete_data)
module Symbolic =
  Make (Dummmy_context) (Symbolic_value) (Constexpr_eval.Symbolic)
    (Symbolic_memory)
    (Symbolic_table)
    (Symbolic_elem)
    (Symbolic_extern.Func)
    (Symbolic_data)

module Abstract = struct
  module Context = struct
    include Abstract_domain.Context

    let empty = Abstract_domain.root_context
  end

  include
    Make (Context) (Abstract_value) (Constexpr_eval.Abstract) (Abstract_memory)
      (Abstract_table)
      (Abstract_elem)
      (Abstract_extern.Func)
      (Abstract_data)
end

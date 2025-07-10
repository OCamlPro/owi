(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Make
    (Memory : Symbolic_memory_intf.S)
    (Thread : Thread_intf.S with type Memory.collection = Memory.collection)
    (Choice :
      Choice_intf.Complete
        with module V := Symbolic_value
         and type thread := Thread.t) =
struct
  module Value = Symbolic_value
  module Choice = Choice
  module Extern_func = Func_intf.Make_extern_func (Value) (Choice) (Memory)
  module Global = Symbolic_global
  module Table = Symbolic_table

  type thread = Thread.t

  let select (c : Value.bool) ~(if_true : Value.t) ~(if_false : Value.t) :
    Value.t Choice.t =
    match (if_true, if_false) with
    | I32 if_true, I32 if_false ->
      Choice.return (Value.I32 (Value.Bool.select_expr c ~if_true ~if_false))
    | I64 if_true, I64 if_false ->
      Choice.return (Value.I64 (Value.Bool.select_expr c ~if_true ~if_false))
    | F32 if_true, F32 if_false ->
      Choice.return (Value.F32 (Value.Bool.select_expr c ~if_true ~if_false))
    | F64 if_true, F64 if_false ->
      Choice.return (Value.F64 (Value.Bool.select_expr c ~if_true ~if_false))
    | Ref _, Ref _ ->
      let open Choice in
      let+ b = select c ~counter_next_true:0 ~counter_next_false:0 in
      if b then if_true else if_false
    | _, _ -> assert false

  let get_pc = Thread.pc

  module Elem = struct
    type t = Link_env.elem

    let get (elem : t) i : Value.ref_value =
      match elem.value.(i) with Funcref f -> Funcref f | _ -> assert false

    let size (elem : t) = Array.length elem.value
  end

  module Memory = struct
    include Memory

    let load_8_s m a = Choice.lift_mem @@ load_8_s m a

    let load_8_u m a = Choice.lift_mem @@ load_8_u m a

    let load_16_s m a = Choice.lift_mem @@ load_16_s m a

    let load_16_u m a = Choice.lift_mem @@ load_16_u m a

    let load_32 m a = Choice.lift_mem @@ load_32 m a

    let load_64 m a = Choice.lift_mem @@ load_64 m a

    let store_8 m ~addr v = Choice.lift_mem @@ store_8 m ~addr v

    let store_16 m ~addr v = Choice.lift_mem @@ store_16 m ~addr v

    let store_32 m ~addr v = Choice.lift_mem @@ store_32 m ~addr v

    let store_64 m ~addr v = Choice.lift_mem @@ store_64 m ~addr v

    let fill m ~pos ~len c = Choice.lift_mem @@ fill m ~pos ~len c

    let blit m ~src ~dst ~len = Choice.lift_mem @@ blit m ~src ~dst ~len
  end

  module Data = struct
    type t = Link_env.data

    let value data = data.Link_env.value

    let size data = String.length data.Link_env.value
  end

  module Env = struct
    type t = Extern_func.extern_func Link_env.t

    type t' = Env_id.t

    let get_memory env id =
      match Link_env.get_memory env id with
      | Error e -> Choice.trap e
      | Ok orig_mem ->
        let f (t : thread) =
          let memories = Thread.memories t in
          Memory.get_memory (Link_env.id env) orig_mem memories id
        in
        Choice.with_thread f

    let get_func = Link_env.get_func

    let get_extern_func = Link_env.get_extern_func

    let get_table (env : t) i : Table.t Choice.t =
      match Link_env.get_table env i with
      | Error e -> Choice.trap e
      | Ok orig_table ->
        let f (t : thread) =
          let tables = Thread.tables t in
          Symbolic_table.get_table (Link_env.id env) orig_table tables i
        in
        Choice.with_thread f

    let get_elem env i = Link_env.get_elem env i

    let get_data env n =
      match Link_env.get_data env n with
      | Error e -> Choice.trap e
      | Ok orig_data -> Choice.return orig_data

    let get_global (env : t) i : Global.t Choice.t =
      match Link_env.get_global env i with
      | Error e -> Choice.trap e
      | Ok orig_global ->
        let f (t : thread) =
          let globals = Thread.globals t in
          Symbolic_global.get_global (Link_env.id env) orig_global globals i
        in
        Choice.with_thread f

    let drop_elem _ =
      (* TODO *)
      ()

    let drop_data = Link_env.drop_data
  end

  module Module_to_run = struct
    (** runnable module *)
    type t =
      { id : string option
      ; env : Env.t
      ; to_run : Types.binary Types.expr Annotated.t list
      }

    let env (t : t) = t.env

    let id (t : t) = t.id

    let to_run (t : t) = t.to_run
  end

  let convert_module_to_run (m : 'f Link.module_to_run) =
    Module_to_run.{ id = m.id; env = m.env; to_run = m.to_run }
end

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module type Thread = sig
  type t

  type memories = Symbolic_memory.collection

  val memories : t -> memories

  val tables : t -> Symbolic_table.collection

  val globals : t -> Symbolic_global.collection

  val pc : t -> Symbolic_value.vbool list
end

module MakeP
    (Thread : Thread)
    (Choice : Choice_intf.Complete
                with module V := Symbolic_value
                 and type thread := Thread.t) =
struct
  module Value = Symbolic_value

  module Choice = struct
    include Choice

    let lift_without_memory (_v : 'a Symbolic_choice_without_memory.t) :
      'a Choice.t =
      Choice.with_thread (fun _t ->
          (* let thread_without_memory = (1* translate _t *1) in *)
          (* let results = *)
          (*   Symbolic_choice_without_memory.run *)
          (*     ~workers:1 *)
          (*     v *)
          (*     thread_without_memory *)
          (*     ~callback *)
          (*     ... *)
          Obj.magic 0 )
  end

  module Extern_func =
    Concrete_value.Make_extern_func (Value) (Choice) (Symbolic_memory)
  module Global = Symbolic_global
  module Table = Symbolic_table

  type thread = Thread.t

  let select (c : Value.vbool) ~(if_true : Value.t) ~(if_false : Value.t) :
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
      let+ b = select c in
      if b then if_true else if_false
    | _, _ -> assert false

  module Elem = struct
    type t = Link_env.elem

    let get (elem : t) i : Value.ref_value =
      match elem.value.(i) with Funcref f -> Funcref f | _ -> assert false

    let size (elem : t) = Array.length elem.value
  end

  module Memory = struct
    include Symbolic_memory

    let concretise (a : Smtml.Expr.t) : Smtml.Expr.t Choice.t =
      let open Choice in
      let open Smtml in
      match Expr.view a with
      (* Avoid unecessary re-hashconsing and allocation when the value
         is already concrete. *)
      | Val _ | Ptr { offset = { node = Val _; _ }; _ } -> return a
      | Ptr { base; offset } ->
        let+ offset = select_i32 offset in
        Expr.ptr base (Symbolic_value.const_i32 offset)
      | _ ->
        let+ v = select_i32 a in
        Symbolic_value.const_i32 v

    let check_within_bounds m a =
      match check_within_bounds m a with
      | Error t -> Choice.trap t
      | Ok (cond, ptr) ->
        let open Choice in
        let* out_of_bounds = select cond in
        if out_of_bounds then trap Trap.Memory_heap_buffer_overflow
        else return ptr

    let with_concrete (m : t) a f : 'a Choice.t =
      let open Choice in
      let* _ = Choice.lift_without_memory @@ address () in
      let* addr = concretise a in
      let+ ptr = check_within_bounds m addr in
      f m ptr

    let load_8_s m a = with_concrete m a load_8_s

    let load_8_u m a = with_concrete m a load_8_u

    let load_16_s m a = with_concrete m a load_16_s

    let load_16_u m a = with_concrete m a load_16_u

    let load_32 m a = with_concrete m a load_32

    let load_64 m a = with_concrete m a load_64

    let store_8 m ~addr v =
      with_concrete m addr (fun m addr -> store_8 m ~addr v)

    let store_16 m ~addr v =
      with_concrete m addr (fun m addr -> store_16 m ~addr v)

    let store_32 m ~addr v =
      with_concrete m addr (fun m addr -> store_32 m ~addr v)

    let store_64 m ~addr v =
      with_concrete m addr (fun m addr -> store_64 m ~addr v)
  end

  module Data = struct
    type t = Link_env.data

    let value data = data.Link_env.value
  end

  module Env = struct
    type t = Extern_func.extern_func Link_env.t

    type t' = Env_id.t

    let get_memory env id =
      let orig_mem = Link_env.get_memory env id in
      let f (t : thread) =
        let memories = Thread.memories t in
        Symbolic_memory.get_memory (Link_env.id env) orig_mem memories id
      in
      Choice.with_thread f

    let get_func = Link_env.get_func

    let get_extern_func = Link_env.get_extern_func

    let get_table (env : t) i : Table.t Choice.t =
      let orig_table = Link_env.get_table env i in
      let f (t : thread) =
        let tables = Thread.tables t in
        Symbolic_table.get_table (Link_env.id env) orig_table tables i
      in
      Choice.with_thread f

    let get_elem env i = Link_env.get_elem env i

    let get_data env n =
      let data = Link_env.get_data env n in
      Choice.return data

    let get_global (env : t) i : Global.t Choice.t =
      let orig_global = Link_env.get_global env i in
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
      { modul : Binary.modul
      ; env : Env.t
      ; to_run : Types.binary Types.expr list
      }

    let env (t : t) = t.env

    let modul (t : t) = t.modul

    let to_run (t : t) = t.to_run
  end
end

module P =
  MakeP [@inlined hint] (Thread_with_memory) (Symbolic_choice_with_memory)
module M =
  MakeP [@inlined hint] (Thread_with_memory) (Symbolic_choice_minimalist)

let convert_module_to_run (m : 'f Link.module_to_run) =
  P.Module_to_run.{ modul = m.modul; env = m.env; to_run = m.to_run }

let convert_module_to_run_minimalist (m : 'f Link.module_to_run) =
  M.Module_to_run.{ modul = m.modul; env = m.env; to_run = m.to_run }

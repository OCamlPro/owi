(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Types
module Def_value = Concrete_value
module Solver = Thread.Solver

module P = struct
  module Value = struct
    include Symbolic_value.S
  end

  type memory = Symbolic_memory.M.t

  type table = Symbolic_table.table

  type elem = Link_env.elem

  type data = Link_env.data

  type global = Symbolic_global.global

  type vbool = Value.vbool

  type int32 = Value.int32

  type int64 = Value.int64

  type float32 = Value.float32

  type float64 = Value.float64

  type thread = Thread.t

  module Choice = Choice_monad.MT
  module Extern_func = Def_value.Make_extern_func (Value) (Choice)

  let select (c : vbool) ~(if_true : Value.t) ~(if_false : Value.t) :
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
      Choice.bind (Choice.select c) (fun b ->
          if b then Choice.return if_true else Choice.return if_false )
    | _, _ -> assert false

  type extern_func = Extern_func.extern_func

  type env = extern_func Link_env.t

  type func = Def_value.Func.t

  module Func = struct
    include Extern_func
  end

  module Global = struct
    type t = global

    let value (v : t) = v.value

    let set_value (v : t) x = v.value <- x

    let mut (v : t) = v.orig.mut

    let typ (v : t) = v.orig.typ
  end

  module Table = struct
    type t = table

    let get t i = t.(i)

    let set t i v = t.(i) <- v

    let size t = Array.length t

    let typ _t =
      (* TODO add type to table *)
      (Null, Func_ht)

    let max_size _t = assert false

    let grow _t _new_size _x = assert false

    let fill _t _pos _len _x = assert false

    let copy ~t_src:_ ~t_dst:_ ~src:_ ~dst:_ ~len:_ = assert false
  end

  module Elem = struct
    type t = elem

    let get (elem : t) i : Value.ref_value =
      match elem.value.(i) with Funcref f -> Funcref f | _ -> assert false

    let size (elem : t) = Array.length elem.value
  end

  module Memory = struct
    include Symbolic_memory.M

    let concretise a =
      let open Expr in
      Choice.with_thread (fun thread ->
          let (S (solver_mod, solver)) = Thread.solver thread in
          let module Solver = (val solver_mod) in
          match a.e with
          | Val _ | Ptr (_, { e = Val _; _ }) -> a
          | Ptr (base, offset) ->
            (* TODO: can we remove this check? We should be in a SAT branch *)
            assert (Solver.check solver @@ Thread.pc thread);
            let concrete_offest = Solver.get_value solver offset in
            Format.pp_std "Concretise: %a = %a@." Expr.pp offset Expr.pp
              concrete_offest;
            let cond = Relop (Eq, offset, concrete_offest) @: a.ty in
            (* TODO: this should go to the pc *)
            Solver.add solver [ cond ];
            Ptr (base, concrete_offest) @: a.ty
          | _ ->
            (* TODO: can we remove this check? We should be in a SAT branch *)
            assert (Solver.check solver @@ Thread.pc thread);
            let concrete_addr = Solver.get_value solver a in
            Format.pp_std "Concretise: %a = %a@." Expr.pp a Expr.pp
              concrete_addr;
            let cond = Relop (Eq, a, concrete_addr) @: a.ty in
            (* TODO: this should go to the pc *)
            Solver.add solver [ cond ];
            concrete_addr )

    (* TODO: *)
    (* 1. Let pointers have symbolic offsets *)
    (* 2. Let addresses have symbolic values *)
    let check_within_bounds m (a : int32) =
      Choice.with_thread (fun thread ->
          let pc = Thread.pc thread in
          let (S (solver_mod, solver)) = Thread.solver thread in
          let module Solver = (val solver_mod) in
          match a.e with
          | Val (Num (I32 _)) -> Ok a
          | Ptr (base, offset) -> (
            match Hashtbl.find m.chunks base with
            | exception Not_found -> Error Trap.Memory_leak_use_after_free
            | size ->
              let ptr = Int32.add base (i32 offset) in
              let upper_bound =
                Value.(I32.ge (const_i32 ptr) (I32.add (const_i32 base) size))
              in
              if ptr < base || Solver.check solver (upper_bound :: pc) then
                Error Trap.Memory_heap_buffer_overflow
              else Ok (Value.const_i32 ptr) )
          | _ -> Log.err {|Unable to calculate address of: "%a"|} Expr.pp a )

    let with_concrete (m : memory) (a : int32) (f : memory -> int32 -> 'a) :
      'a Choice.t =
      Choice.bind (concretise a) (fun addr ->
          Choice.bind (check_within_bounds m addr) (function
            | Ok a -> Choice.return @@ f m a
            | Error t -> Choice.trap t ) )

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
    type t = data

    let value data = data.Link_env.value
  end

  module Env = struct
    type t = env

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
      (* TODO ? *)
      ()

    let drop_data = Link_env.drop_data

    let pp _ _ = ()
  end

  module Module_to_run = struct
    (** runnable module *)
    type t =
      { modul : Simplified.modul
      ; env : Env.t
      ; to_run : simplified expr list
      }

    let env (t : t) = t.env

    let modul (t : t) = t.modul

    let to_run (t : t) = t.to_run
  end
end

module P' : Interpret_functor_intf.P = P

let convert_module_to_run (m : 'f Link.module_to_run) =
  P.Module_to_run.{ modul = m.modul; env = m.env; to_run = m.to_run }

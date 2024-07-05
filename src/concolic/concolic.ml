(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module P = struct
  type thread = Concolic_choice.thread

  module Value = Concolic_value.V

  module Choice' : Choice_intf.Base with module V := Value = Concolic_choice

  module Choice = Concolic_choice

  let select (c : Value.vbool) ~(if_true : Value.t) ~(if_false : Value.t) :
    Value.t Choice.t =
    (* TODO / Think: this should probably be an ite expression in the symbolic part ? *)
    let select if_true if_false =
      if c.concrete then if_true.Concolic_value.concrete
      else if_false.Concolic_value.concrete
    in
    match (if_true, if_false) with
    | I32 if_true, I32 if_false ->
      Choice.return
      @@ Value.I32
           { symbolic =
               Symbolic_value.Bool.select_expr c.symbolic
                 ~if_true:if_true.symbolic ~if_false:if_false.symbolic
           ; concrete = select if_true if_false
           }
    | I64 if_true, I64 if_false ->
      Choice.return
      @@ Value.I64
           { symbolic =
               Symbolic_value.Bool.select_expr c.symbolic
                 ~if_true:if_true.symbolic ~if_false:if_false.symbolic
           ; concrete = select if_true if_false
           }
    | F32 if_true, F32 if_false ->
      Choice.return
      @@ Value.F32
           { symbolic =
               Symbolic_value.Bool.select_expr c.symbolic
                 ~if_true:if_true.symbolic ~if_false:if_false.symbolic
           ; concrete = select if_true if_false
           }
    | F64 if_true, F64 if_false ->
      Choice.return
      @@ Value.F64
           { symbolic =
               Symbolic_value.Bool.select_expr c.symbolic
                 ~if_true:if_true.symbolic ~if_false:if_false.symbolic
           ; concrete = select if_true if_false
           }
    | Ref _, Ref _ ->
      (* Concretization: add something to the PC *)
      failwith "TODO"
    | _, _ -> assert false

  module Global = struct
    open Concolic_value

    type t = (Concrete_global.t, Symbolic_global.t) cs

    let value (g : t) : Value.t =
      Concolic_value.V.value_pair
        (Concrete_global.value g.concrete)
        (Symbolic_global.value g.symbolic)

    let set_value (g : t) cs =
      Concrete_global.set_value g.concrete (Concolic_value.V.concrete_value cs);
      Symbolic_global.set_value g.symbolic (Concolic_value.V.symbolic_value cs)

    let mut (g : t) = Concrete_global.mut g.concrete

    let typ (g : t) = Concrete_global.typ g.concrete
  end

  module Table = struct
    open Concolic_value

    type t = (Concrete_table.t, Symbolic_table.t) cs

    let get t i =
      Concolic_value.V.pair
        (Concrete_table.get t.concrete i)
        (Symbolic_table.get t.symbolic i)

    let set t i v =
      Concrete_table.set t.concrete i v.concrete;
      Symbolic_table.set t.symbolic i v.symbolic

    let size t = Concrete_table.size t.concrete

    let typ t = Concrete_table.typ t.concrete

    let max_size t = Concrete_table.max_size t.concrete

    let grow t new_size x =
      Concrete_table.grow t.concrete new_size x.concrete;
      Symbolic_table.grow t.symbolic new_size x.symbolic

    let fill t pos len x =
      Concrete_table.fill t.concrete pos len x.concrete;
      Symbolic_table.fill t.symbolic pos len x.symbolic

    let copy ~t_src ~t_dst ~src ~dst ~len =
      Concrete_table.copy ~t_src:t_src.concrete ~t_dst:t_dst.concrete ~src ~dst
        ~len;
      Symbolic_table.copy ~t_src:t_src.symbolic ~t_dst:t_dst.symbolic ~src ~dst
        ~len
  end

  module Memory = struct
    open Concolic_value

    type t = (Concrete_memory.t, Symbolic_memory.t) cs

    module C = Concrete_memory
    module S = Symbolic_memory

    let with_concrete m a f_c f_s =
      let open Choice in
      let+ a = Choice.select_i32 a in
      { concrete = f_c m.concrete a
      ; symbolic = f_s m.symbolic (Symbolic_value.const_i32 a)
      }

    let with_concrete_store m a f_c f_s v =
      let open Choice in
      let+ addr = Choice.select_i32 a in
      f_c m.concrete ~addr v.concrete;
      f_s m.symbolic ~addr:(Symbolic_value.const_i32 addr) v.symbolic

    let load_8_s m a = with_concrete m a C.load_8_s S.load_8_s

    let load_8_u m a = with_concrete m a C.load_8_u S.load_8_u

    let load_16_s m a = with_concrete m a C.load_16_s S.load_16_s

    let load_16_u m a = with_concrete m a C.load_16_u S.load_16_u

    let load_32 m a = with_concrete m a C.load_32 S.load_32

    let load_64 m a = with_concrete m a C.load_64 S.load_64

    let store_8 m ~addr v = with_concrete_store m addr C.store_8 S.store_8 v

    let store_16 m ~addr v = with_concrete_store m addr C.store_16 S.store_16 v

    let store_32 m ~addr v = with_concrete_store m addr C.store_32 S.store_32 v

    let store_64 m ~addr v = with_concrete_store m addr C.store_64 S.store_64 v

    let grow m delta =
      Concrete_memory.grow m.concrete delta.concrete;
      Symbolic_memory.grow m.symbolic delta.symbolic

    let size m =
      { concrete = Concrete_memory.size m.concrete
      ; symbolic = Symbolic_memory.size m.symbolic
      }

    let size_in_pages m =
      { concrete = Concrete_memory.size_in_pages m.concrete
      ; symbolic = Symbolic_memory.size_in_pages m.symbolic
      }

    let fill _ = assert false

    let blit _ = assert false

    let blit_string m s ~src ~dst ~len =
      { concrete =
          Concrete_memory.blit_string m.concrete s ~src:src.concrete
            ~dst:dst.concrete ~len:len.concrete
      ; symbolic =
          Symbolic_memory.blit_string m.symbolic s ~src:src.symbolic
            ~dst:dst.symbolic ~len:len.symbolic
      }

    let get_limit_max _ = failwith "TODO"
  end

  module Extern_func = Concrete_value.Make_extern_func (Value) (Choice) (Memory)

  module Data = struct
    type t = Link_env.data

    let value data = data.Link_env.value
  end

  module Elem = struct
    type t = Link_env.elem

    let get (elem : t) i : Value.ref_value =
      match elem.value.(i) with
      | Funcref f -> { concrete = Funcref f; symbolic = Funcref f }
      | _ -> assert false

    let size (elem : t) = Array.length elem.value
  end

  module Env = struct
    type t = Extern_func.extern_func Link_env.t

    type t' = Env_id.t

    let get_memory env id : Memory.t Choice.t =
      let orig_mem = Link_env.get_memory env id in
      let f (t : thread) : Memory.t =
        let sym_mem =
          Symbolic_memory.get_memory (Link_env.id env) orig_mem
            t.shared.memories id
        in
        { concrete = orig_mem; symbolic = sym_mem }
      in
      Choice.with_thread f

    let get_func env id = Link_env.get_func env id

    let get_extern_func env id = Link_env.get_extern_func env id

    let get_table env id : Table.t Choice.t =
      let orig_table = Link_env.get_table env id in
      let f (t : thread) : Table.t =
        let sym_table =
          Symbolic_table.get_table (Link_env.id env) orig_table t.shared.tables
            id
        in
        { concrete = orig_table; symbolic = sym_table }
      in
      Choice.with_thread f

    let get_elem env i = Link_env.get_elem env i

    let get_data env n =
      let data = Link_env.get_data env n in
      Choice.return data

    let get_global env id : Global.t Choice.t =
      let orig_global = Link_env.get_global env id in
      let f (t : thread) : Global.t =
        let sym_global =
          Symbolic_global.get_global (Link_env.id env) orig_global
            t.shared.globals id
        in
        { concrete = orig_global; symbolic = sym_global }
      in
      Choice.with_thread f

    let drop_elem _ =
      (* TODO *)
      ()

    let drop_data = Link_env.drop_data
  end

  module Module_to_run = struct
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

module P' : Interpret_intf.P = P

let convert_module_to_run (m : 'f Link.module_to_run) =
  P.Module_to_run.{ modul = m.modul; env = m.env; to_run = m.to_run }

let backup (m : P.Module_to_run.t) = Link_env.backup m.env

let recover b (m : P.Module_to_run.t) = Link_env.recover b m.env

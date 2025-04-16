(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type thread = Concolic_choice.thread

module Value = Concolic_value
module Choice = Concolic_choice
module Memory = Concolic_memory
module Extern_func = Concolic_extern_func

let select ((c, s) : Value.bool) ~(if_true : Value.t) ~(if_false : Value.t) :
  Value.t Choice.t =
  (* TODO / Think: this should probably be an ite expression in the symbolic part ? *)
  let select if_true if_false = if c then fst if_true else fst if_false in
  begin
    match (if_true, if_false) with
    | I32 if_true, I32 if_false ->
      Value.I32
        ( select if_true if_false
        , Symbolic_value.Bool.select_expr s ~if_true:(snd if_true)
            ~if_false:(snd if_false) )
    | I64 if_true, I64 if_false ->
      Value.I64
        ( select if_true if_false
        , Symbolic_value.Bool.select_expr s ~if_true:(snd if_true)
            ~if_false:(snd if_false) )
    | F32 if_true, F32 if_false ->
      Value.F32
        ( select if_true if_false
        , Symbolic_value.Bool.select_expr s ~if_true:(snd if_true)
            ~if_false:(snd if_false) )
    | F64 if_true, F64 if_false ->
      Value.F64
        ( select if_true if_false
        , Symbolic_value.Bool.select_expr s ~if_true:(snd if_true)
            ~if_false:(snd if_false) )
    | Ref _, Ref _ ->
      (* Concretization: add something to the PC *)
      Fmt.failwith "TODO"
    | _, _ -> assert false
  end
  |> Choice.return

module Global = struct
  type t = Concrete_global.t * Symbolic_global.t

  let value ((c, s) : t) : Value.t =
    Value.pair_value (Concrete_global.value c) (Symbolic_global.value s)

  let set_value ((c, s) : t) cs =
    Concrete_global.set_value c (Value.concrete_value cs);
    Symbolic_global.set_value s (Value.symbolic_value cs)

  let mut ((c, _s) : t) = Concrete_global.mut c

  let typ ((c, _s) : t) = Concrete_global.typ c
end

module Table = struct
  type t = Concrete_table.t * Symbolic_table.t

  let get (c, s) i = (Concrete_table.get c i, Symbolic_table.get s i)

  let set (tc, ts) i (vc, vs) =
    Concrete_table.set tc i vc;
    Symbolic_table.set ts i vs

  let size (c, _s) = Concrete_table.size c

  let typ (c, _s) = Concrete_table.typ c

  let max_size (c, _s) = Concrete_table.max_size c

  let grow (tc, ts) new_size (xc, xs) =
    Concrete_table.grow tc new_size xc;
    Symbolic_table.grow ts new_size xs

  let fill (tc, ts) pos len (xc, xs) =
    Concrete_table.fill tc pos len xc;
    Symbolic_table.fill ts pos len xs

  let copy ~t_src ~t_dst ~src ~dst ~len =
    let t_srcc, t_srcs = t_src in
    let t_dstc, t_dsts = t_dst in
    Concrete_table.copy ~t_src:t_srcc ~t_dst:t_dstc ~src ~dst ~len;
    Symbolic_table.copy ~t_src:t_srcs ~t_dst:t_dsts ~src ~dst ~len
end

module Data = struct
  type t = Link_env.data

  let value data = data.Link_env.value
end

module Elem = struct
  type t = Link_env.elem

  let get (elem : t) i : Value.ref_value =
    match elem.value.(i) with
    | Funcref f -> (Funcref f, Funcref f)
    | _ -> assert false

  let size (elem : t) = Array.length elem.value
end

module Env = struct
  type t = Extern_func.extern_func Link_env.t

  let get_memory env id : Memory.t Choice.t =
    match Link_env.get_memory env id with
    | Error t -> Choice.trap t
    | Ok orig_mem ->
      let f (t : thread) : Memory.t =
        let sym_mem =
          Symbolic_memory.get_memory (Link_env.id env) orig_mem
            t.shared.memories id
        in
        (orig_mem, sym_mem)
      in
      Choice.with_thread f

  let get_func env id = Link_env.get_func env id

  let get_extern_func env id = Link_env.get_extern_func env id

  let get_table env id : Table.t Choice.t =
    match Link_env.get_table env id with
    | Error t -> Choice.trap t
    | Ok orig_table ->
      let f (t : thread) : Table.t =
        let sym_table =
          Symbolic_table.get_table (Link_env.id env) orig_table t.shared.tables
            id
        in
        (orig_table, sym_table)
      in
      Choice.with_thread f

  let get_elem env i = Link_env.get_elem env i

  let get_data env n =
    match Link_env.get_data env n with
    | Error t -> Choice.trap t
    | Ok data -> Choice.return data

  let get_global env id : Global.t Choice.t =
    match Link_env.get_global env id with
    | Error t -> Choice.trap t
    | Ok orig_global ->
      let f (t : thread) : Global.t =
        let sym_global =
          Symbolic_global.get_global (Link_env.id env) orig_global
            t.shared.globals id
        in
        (orig_global, sym_global)
      in
      Choice.with_thread f

  let drop_elem _ =
    (* TODO *)
    ()

  let drop_data = Link_env.drop_data
end

module Module_to_run = struct
  type t =
    { id : string option
    ; env : Env.t
    ; to_run : Types.binary Types.expr list
    }

  let env (t : t) = t.env

  let id (t : t) = t.id

  let to_run (t : t) = t.to_run
end

let convert_module_to_run (m : 'f Link.module_to_run) =
  Module_to_run.{ id = m.id; env = m.env; to_run = m.to_run }

let backup (m : Module_to_run.t) = Link_env.backup m.env

let recover b (m : Module_to_run.t) = Link_env.recover b m.env

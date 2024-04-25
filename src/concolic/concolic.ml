module P = struct
  type thread = Concolic_choice.thread

  module Value = Concolic_value.V

  module Choice' : Choice_intf.Base with module V := Value = Concolic_choice

  module Choice = Concolic_choice

  let select (c : Value.vbool) ~(if_true : Value.t) ~(if_false : Value.t) :
    Value.t Choice.t =
    (* TODO / Think: this should probably be an ite expression in the symbolic part ? *)
    let select if_true if_false =
      if c.c then if_true.Concolic_value.c else if_false.Concolic_value.c
    in
    match (if_true, if_false) with
    | I32 if_true, I32 if_false ->
      Choice.return
      @@ Value.I32
           { s =
               Symbolic_value.Bool.select_expr c.s ~if_true:if_true.s
                 ~if_false:if_false.s
           ; c = select if_true if_false
           }
    | I64 if_true, I64 if_false ->
      Choice.return
      @@ Value.I64
           { s =
               Symbolic_value.Bool.select_expr c.s ~if_true:if_true.s
                 ~if_false:if_false.s
           ; c = select if_true if_false
           }
    | F32 if_true, F32 if_false ->
      Choice.return
      @@ Value.F32
           { s =
               Symbolic_value.Bool.select_expr c.s ~if_true:if_true.s
                 ~if_false:if_false.s
           ; c = select if_true if_false
           }
    | F64 if_true, F64 if_false ->
      Choice.return
      @@ Value.F64
           { s =
               Symbolic_value.Bool.select_expr c.s ~if_true:if_true.s
                 ~if_false:if_false.s
           ; c = select if_true if_false
           }
    | Ref _, Ref _ ->
      (* Concretization: add something to the PC *)
      failwith "TODO"
    | _, _ -> assert false

  module Extern_func = Concrete_value.Make_extern_func (Value) (Choice)

  module Global = struct
    open Concolic_value

    type t = (Concrete_global.t, Symbolic_global.t) cs

    let value (g : t) : Value.t =
      Concolic_value.V.value_pair
        (Concrete_global.value g.c)
        (Symbolic_global.value g.s)

    let set_value (g : t) cs =
      Concrete_global.set_value g.c (Concolic_value.V.concrete_value cs);
      Symbolic_global.set_value g.s (Concolic_value.V.symbolic_value cs)

    let mut (g : t) = Concrete_global.mut g.c

    let typ (g : t) = Concrete_global.typ g.c
  end

  module Table = struct
    open Concolic_value

    type t = (Concrete_table.t, Symbolic_table.t) cs

    let get _t _i = failwith "TODO"

    let set _t _i _v = failwith "TODO"

    let size t = Concrete_table.size t.c

    let typ t = Concrete_table.typ t.c

    let max_size t = Concrete_table.max_size t.c

    let grow _t _new_size _x = failwith "TODO"

    let fill _t _pos _len _x = failwith "TODO"

    let copy ~t_src:_ ~t_dst:_ ~src:_ ~dst:_ ~len:_ = failwith "TODO"
  end

  module Memory = struct
    open Concolic_value

    type t = (Concrete_memory.t, Symbolic_memory.t) cs

    module C = Concrete_memory
    module S = Symbolic_memory

    let with_concrete m a f_c f_s =
      let open Choice in
      let+ a = Choice.select_i32 a in
      { c = f_c m.c a; s = f_s m.s (Symbolic_value.const_i32 a) }

    let with_concrete_store m a f_c f_s v =
      let open Choice in
      let+ addr = Choice.select_i32 a in
      f_c m.c ~addr v.c;
      f_s m.s ~addr:(Symbolic_value.const_i32 addr) v.s

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
      Concrete_memory.grow m.c delta.c;
      Symbolic_memory.grow m.s delta.s

    let size m = { c = Concrete_memory.size m.c; s = Symbolic_memory.size m.s }

    let size_in_pages m =
      { c = Concrete_memory.size_in_pages m.c
      ; s = Symbolic_memory.size_in_pages m.s
      }

    let fill _ = assert false

    let blit _ = assert false

    let blit_string m s ~src ~dst ~len =
      { c = Concrete_memory.blit_string m.c s ~src:src.c ~dst:dst.c ~len:len.c
      ; s = Symbolic_memory.blit_string m.s s ~src:src.s ~dst:dst.s ~len:len.s
      }

    let get_limit_max _ = failwith "TODO"
  end

  module Data = struct
    type t = Link_env.data

    let value data = data.Link_env.value
  end

  module Elem = struct
    type t = Link_env.elem

    let get (elem : t) i : Value.ref_value =
      match elem.value.(i) with
      | Funcref f -> { c = Funcref f; s = Funcref f }
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
        { c = orig_mem; s = sym_mem }
      in
      Choice.with_thread f

    let get_func env id = Link_env.get_func env id

    let get_extern_func env id = Link_env.get_extern_func env id

    let get_table env id : Table.t Choice.t =
      let orig_table = Link_env.get_table env id in
      let f (t : thread) : Table.t =
        let sym_table =
          Symbolic_table.get_table (Link_env.id env) orig_table t.shared.tables id
        in
        { c = orig_table; s = sym_table }
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
          Symbolic_global.get_global (Link_env.id env) orig_global t.shared.globals id
        in
        { c = orig_global; s = sym_global }
      in
      Choice.with_thread f

    let drop_elem _ =
      (* TODO *)
      ()

    let drop_data = Link_env.drop_data
  end

  module Module_to_run = struct
    type t =
      { modul : Simplified.modul
      ; env : Env.t
      ; to_run : Types.simplified Types.expr list
      }

    let env (t : t) = t.env

    let modul (t : t) = t.modul

    let to_run (t : t) = t.to_run
  end
end

module P' : Interpret_intf.P = P

let convert_module_to_run (m : 'f Link.module_to_run) =
  P.Module_to_run.
    { modul = m.modul; env = m.env; to_run = m.to_run }

module Def_value = Value
module Solver = Thread.Solver

module P = struct
  module Value = struct
    include Sym_value.S
  end

  type memory = Sym_memory.M.t

  type table = Sym_table.table

  type elem = Link.Env.elem

  type data = Link.Env.data

  type global = unit

  type vbool = Value.vbool

  type int32 = Value.int32

  type int64 = Value.int64

  type float32 = Value.float32

  type float64 = Value.float64

  type thread = Thread.t

  module Choice = Choice_monad.Explicit
  module Extern_func = Def_value.Make_extern_func (Value) (Choice)

  type extern_func = Extern_func.extern_func

  type env = extern_func Link_env.t

  type func = Def_value.Func.t

  module Func = struct
    include Extern_func
  end

  module Global = struct
    type t = global

    let value _ = assert false

    let set_value _ = assert false

    let mut _ = assert false

    let typ _ = assert false
  end

  module Table = struct
    type t = table

    let get t i = t.(i)

    let set t i v = t.(i) <- v

    let size t = Array.length t
  end

  module Elem = struct
    type t = elem

    let get (elem : t) i : Value.ref_value =
      match elem.value.(i) with Funcref f -> Funcref f | _ -> assert false

    let size (elem : t) = Array.length elem.value
  end

  module Memory = struct
    include Sym_memory.M
  end

  module Data = struct
    type t = data

    let value data = data.Link_env.value
  end

  module Env = struct
    type t = env

    type t' = Env_id.t

    let get_memory _env _ = Ok (Choice.with_thread Thread.mem)

    let get_func = Link_env.get_func

    let get_extern_func = Link_env.get_extern_func

    let get_table (env : t) i : Table.t Choice.t Result.t =
      match Link_env.get_table env i with
      | Error _ as e -> e
      | Ok orig_table ->
        let f (t : thread) =
          let tables = Thread.tables t in
          Sym_table.get_table (Link_env.id env) orig_table tables i
        in
        Ok (Choice.with_thread f)

    let get_elem env i = Link_env.get_elem env i

    let get_data = Link_env.get_data

    let get_global _ = assert false

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
      ; to_run : Simplified.expr list
      }

    let env (t : t) = t.env

    let modul (t : t) = t.modul

    let to_run (t : t) = t.to_run
  end
end

module P' : Interpret_functor_intf.P = P

let convert_module_to_run (m : 'f Link.module_to_run) =
  P.Module_to_run.{ modul = m.modul; env = m.env; to_run = m.to_run }

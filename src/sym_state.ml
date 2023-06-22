let trap msg = raise (Types.Trap msg)

let ( let* ) o f = Result.fold ~ok:f ~error:trap o

module Def_value = Value

module P = struct
  module Value = struct
    include Sym_value.Symbolic
  end

  type env = Link_env.t

  type memory = unit

  type 'env table = unit

  type 'env elem = unit

  type data = unit

  type 'env global = unit

  type vbool = Value.vbool

  type int32 = Value.int32

  type int64 = Value.int64

  type float32 = Value.float32

  type float64 = Value.float64

  type thread = Value.vbool list

  module Choice = struct
    type 'a t = thread -> ('a * thread)

    let return (v: 'a) : 'a t = (fun t -> v, t)

    let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
      (fun t ->
         let r, t = v t in
         (f r) t)

    let select (sym_bool : vbool) : bool t =
      fun t ->
      let sym_bool = Encoding.Expression.simplify sym_bool in
      let r =
        match sym_bool with
        | Val (Bool b) -> b
        | Val (Num (I32 0l)) -> false
        | Val (Num (I32 _)) -> true
        | _ ->
          Format.printf "%s@." (Encoding.Expression.to_string sym_bool);
          assert false
      in
      let pc = Encoding.Expression.Val (Bool r) in
      r, pc :: t

    let select_i32 _sym_int = assert false

    let get : thread t = (fun t -> t, t)

    let trap : Interpret_functor_intf.trap -> 'a t = function
      | Out_of_bound_memory_access -> assert false
      | Integer_overflow -> assert false
      | Integer_divide_by_zero -> assert false

    (* raise (Types.Trap "out of bounds memory access") *)
  end

  module Extern_func = Def_value.Make_extern_func (Value)

  type extern_func = Extern_func.extern_func

  type func = Def_value.Func.t

  module Func = struct
      include Extern_func
  end

  module Global = struct
    type 'env t = 'env global

    let value _ = assert false

    let set_value _ = assert false

    let mut _ = assert false

    let typ _ = assert false
  end

  module Table = struct
    type 'env t = 'env table

    let get _ = assert false

    let set _ = assert false

    let size _ = assert false
  end

  module Memory = struct
    type t = memory

    let load_8_s _ = assert false

    let load_8_u _ = assert false

    let load_16_s _ = assert false

    let load_16_u _ = assert false

    let load_32 _ = assert false

    let load_64 _ = assert false

    let store_8 _ = assert false

    let store_16 _ = assert false

    let store_32 _ = assert false

    let store_64 _ = assert false

    let grow _ = assert false

    let size _ = assert false

    let size_in_pages _ = assert false
  end

  module Env = struct
    type t = env

    type t' = Env_id.t

    let get_memory _ = assert false

    let get_func t id =
      let* func = Link_env.get_func t id in
      match func with
      | WASM (id, f, env) -> Ok (Func_intf.WASM (id, f, env))
      | Extern _ -> assert false

    let get_table _ = assert false

    let get_elem _ = assert false

    let get_data _ = assert false

    let get_global _ = assert false

    let drop_elem _ = assert false

    let drop_data _ = assert false

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

let convert_module_to_run (m : Link.module_to_run) =
  P.Module_to_run.{ modul = m.modul; env = m.env; to_run = m.to_run }

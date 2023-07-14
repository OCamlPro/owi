module Solver = Encoding.Batch.Make (Encoding.Z3_mappings)
module Def_value = Value

module P = struct
  module Value = struct
    include Sym_value.S
  end

  type memory = Sym_memory.M.t

  type table = unit

  type elem = unit

  type data = Link.Env.data

  type global = unit

  type vbool = Value.vbool

  type int32 = Value.int32

  type int64 = Value.int64

  type float32 = Value.float32

  type float64 = Value.float64

  type thread =
    { solver : Solver.t
    ; pc : Value.vbool list
    ; mem : memory
    }

  module Choice_once = struct
    type 'a t = thread -> 'a * thread

    let return (v : 'a) : 'a t = fun t -> (v, t)

    let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
     fun t ->
      let r, t = v t in
      (f r) t

    let select (sym_bool : vbool) : bool t =
     fun ({ solver; pc = path_condition; _ } as state) ->
      let sym_bool = Encoding.Expression.simplify sym_bool in
      match sym_bool with
      | Val (Bool b) -> (b, state)
      | Val (Num (I32 0l)) -> (false, state)
      | Val (Num (I32 _)) -> (true, state)
      | _ ->
        let value, path_condition =
          if Solver.check solver (sym_bool :: path_condition) then
            (true, sym_bool :: path_condition)
          else
            let no = Value.Bool.not sym_bool in
            if Solver.check solver (no :: path_condition) then
              (false, no :: path_condition)
            else assert false
        in
        Log.debug1 "%s@." (Encoding.Expression.to_string sym_bool);
        (value, { state with pc = path_condition })

    let select_i32 _sym_int = assert false

    let get : thread t = fun t -> (t, t)

    let trap : Trap.t -> 'a t = function
      | Out_of_bounds_table_access -> assert false
      | Out_of_bounds_memory_access -> assert false
      | Integer_overflow -> assert false
      | Integer_divide_by_zero -> assert false
      | Unreachable -> assert false
  end

  module Choice_list = struct
    type 'a t = thread -> ('a * thread) list

    let return (v : 'a) : 'a t = fun t -> [ (v, t) ]

    let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
     fun t ->
      let lst = v t in
      List.flatten @@ List.map (fun (r, t) -> (f r) t) lst

    let select (sym_bool : vbool) : bool t =
     fun ({ solver; pc; mem } as state) ->
      let sym_bool = Encoding.Expression.simplify sym_bool in
      match sym_bool with
      | Val (Bool b) -> [ (b, state) ]
      | Val (Num (I32 _)) -> assert false
      | _ -> (
        let no = Value.Bool.not sym_bool in
        let sat_true = Solver.check solver [ sym_bool ] in
        let sat_false = Solver.check solver [ no ] in
        match (sat_true, sat_false) with
        | false, false -> []
        | true, false -> [ (true, state) ]
        | false, true -> [ (false, state) ]
        | true, true ->
          let solver' = Solver.clone solver in
          Solver.add solver [ sym_bool ];
          Solver.add solver' [ no ];
          let state1 =
            { state with pc = sym_bool :: pc; mem = Sym_memory.M.clone mem }
          in
          let state2 =
            { solver = solver'; pc = no :: pc; mem = Sym_memory.M.clone mem }
          in
          [ (true, state1); (false, state2) ] )

    let select_i32 _sym_int = assert false

    let get : thread t = fun t -> [ (t, t) ]

    let trap : Trap.t -> 'a t = function
      | Out_of_bounds_table_access -> assert false
      | Out_of_bounds_memory_access -> assert false
      | Integer_overflow -> assert false
      | Integer_divide_by_zero -> assert false
      | Unreachable -> fun _ -> []

    (* raise (Types.Trap "out of bounds memory access") *)
  end

  module Choice = Choice_list
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

    let get _ = assert false

    let set _ = assert false

    let size _ = assert false
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

    let get_memory _env _ = Ok (fun t -> [ (t.mem, t) ])

    let get_func = Link_env.get_func

    let get_extern_func = Link_env.get_extern_func

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

let convert_module_to_run (m : 'f Link.module_to_run) =
  P.Module_to_run.{ modul = m.modul; env = m.env; to_run = m.to_run }

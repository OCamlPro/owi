open Choice_monad_intf

type vbool = Sym_value.S.vbool

let eval_choice (sym_bool : vbool) (state : Thread.t) : (bool * Thread.t) list =
  let solver = Thread.solver state in
  let pc = Thread.pc state in
  let memories = Thread.memories state in
  let tables = Thread.tables state in
  let globals = Thread.globals state in
  let sym_bool = Encoding.Expression.simplify sym_bool in
  match sym_bool with
  | Val (Bool b) -> [ (b, state) ]
  | Val (Num (I32 _)) -> assert false
  | _ -> (
    let no = Sym_value.S.Bool.not sym_bool in
    let sat_true = Thread.Solver.check solver (sym_bool :: pc) in
    let sat_false = Thread.Solver.check solver (no :: pc) in
    match (sat_true, sat_false) with
    | false, false -> []
    | true, false -> [ (true, state) ]
    | false, true -> [ (false, state) ]
    | true, true ->
      Format.printf "CHOICE: %s@." (Encoding.Expression.to_string sym_bool);
      let state1 : Thread.t =
        { solver
        ; pc = sym_bool :: pc
        ; memories = Sym_memory.clone memories
        ; tables = Sym_table.clone tables
        ; globals = Sym_global.clone globals
        }
      in
      let state2 : Thread.t =
        { solver
        ; pc = no :: pc
        ; memories = Sym_memory.clone memories
        ; tables = Sym_table.clone tables
        ; globals = Sym_global.clone globals
        }
      in
      [ (true, state1); (false, state2) ] )

module List = struct
  type vbool = Sym_value.S.vbool

  type thread = Thread.t

  type 'a t = thread -> ('a * thread) list

  let return (v : 'a) : 'a t = fun t -> [ (v, t) ]

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let lst = v t in
    match lst with
    | [] -> []
    | [ (r, t) ] -> (f r) t
    | _ -> List.concat_map (fun (r, t) -> (f r) t) lst

  let select (sym_bool : vbool) : bool t = eval_choice sym_bool

  let select_i32 (i : Sym_value.S.int32) : int32 t =
    match i with Val (Num (I32 v)) -> return v | _ -> assert false

  let trap : Trap.t -> 'a t = function
    | Out_of_bounds_table_access -> assert false
    | Out_of_bounds_memory_access -> assert false
    | Integer_overflow -> assert false
    | Integer_divide_by_zero -> assert false
    | Unreachable -> fun _ -> []

  (* raise (Types.Trap "out of bounds memory access") *)

  let with_thread (f : thread -> 'b) : 'b t = fun t -> [ (f t, t) ]

  let add_pc (c : Sym_value.S.vbool) : unit t =
   fun t -> [ ((), { t with pc = c :: t.pc }) ]

  let run (v : 'a t) (thread : thread) = List.to_seq (v thread)
end

module Seq = struct
  module List = Stdlib.List

  type vbool = Sym_value.S.vbool

  type thread = Thread.t

  type 'a t = thread -> ('a * thread) Seq.t

  let return (v : 'a) : 'a t = fun t -> Seq.return (v, t)

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let seq = v t in
    Seq.flat_map (fun (e, t) -> f e t) seq

  let select (sym_bool : vbool) : bool t =
   fun state -> List.to_seq (eval_choice sym_bool state)

  let select_i32 (i : Sym_value.S.int32) : int32 t =
    match i with Val (Num (I32 v)) -> return v | _ -> assert false

  let trap : Trap.t -> 'a t = function
    | Out_of_bounds_table_access -> assert false
    | Out_of_bounds_memory_access -> assert false
    | Integer_overflow -> assert false
    | Integer_divide_by_zero -> assert false
    | Unreachable -> fun _ -> Seq.empty

  (* raise (Types.Trap "out of bounds memory access") *)

  let with_thread (f : thread -> 'b) : 'b t = fun t -> Seq.return (f t, t)

  let add_pc (c : Sym_value.S.vbool) : unit t =
   fun t -> Seq.return ((), { t with pc = c :: t.pc })

  let run (v : 'a t) (thread : thread) = v thread
end

module Explicit = struct
  module List = Stdlib.List
  module Seq = Stdlib.Seq

  type vbool = Sym_value.S.vbool

  type thread = Thread.t

  type 'a st = St of (thread -> 'a * thread) [@@unboxed]

  type 'a t =
    | Empty : 'a t
    | Ret : 'a st -> 'a t
    | Retv : 'a -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Choice : vbool -> bool t
    | Trap : Trap.t -> 'a t

  let return (v : 'a) : 'a t = Retv v [@@inline]

  let bind : type a b. a t -> (a -> b t) -> b t =
   fun v f ->
    match v with
    | Empty -> Empty
    | Trap t -> Trap t
    | Retv v -> f v
    | Ret _ | Choice _ -> Bind (v, f)
    | Bind _ -> Bind (v, f)
   [@@inline]

  (* let rec bind : type a b. a t -> (a -> b t) -> b t =
   *  fun v f ->
   *   match v with
   *   | Empty -> Empty
   *   | Retv v -> f v
   *   | Ret _ | Choice _ -> Bind (v, f)
   *   | Bind (v, f1) -> Bind (v, fun x -> bind (f1 x) f)
   *  [@@inline] *)

  let select (cond : vbool) : bool t =
    match cond with Val (Bool b) -> Retv b | _ -> Choice cond
    [@@inline]

  let select_i32 (i : Sym_value.S.int32) : int32 t =
    match i with Val (Num (I32 v)) -> Retv v | _ -> assert false

  let trap : Trap.t -> 'a t = fun t -> Trap t

  let with_thread (f : thread -> 'b) : 'b t = Ret (St (fun t -> (f t, t)))
    [@@inline]

  let add_pc (c : Sym_value.S.vbool) : unit t =
    Ret (St (fun t -> ((), { t with pc = c :: t.pc })))
    [@@inline]

  let rec run : type a. a t -> thread -> (a * thread) Seq.t =
   fun v t ->
    match v with
    | Empty -> Seq.empty
    | Trap _t -> Seq.empty (* TODO do something useful with the trap *)
    | Retv v -> Seq.return (v, t)
    | Ret (St f) -> Seq.return (f t)
    | Bind (v, f) -> Seq.flat_map (fun (v, t) -> run (f v) t) (run v t)
    | Choice cond -> List.to_seq (eval_choice cond t)

  let rec run_and_trap : type a. a t -> thread -> (a eval * thread) Seq.t =
   fun v t ->
    match v with
    | Empty -> Seq.empty
    | Trap tr -> Seq.return (ETrap tr, t)
    | Retv v -> Seq.return (EVal v, t)
    | Ret (St f) ->
      let v, t = f t in
      Seq.return (EVal v, t)
    | Bind (v, f) ->
      Seq.flat_map
        (fun (v, t) ->
          match v with
          | ETrap tr -> Seq.return (ETrap tr, t)
          | EVal v -> run_and_trap (f v) t )
        (run_and_trap v t)
    | Choice cond ->
      List.to_seq (List.map (fun (v, t) -> (EVal v, t)) (eval_choice cond t))

  let rec run_up_to : type a. depth:int -> a t -> thread -> (a * thread) Seq.t =
   fun ~depth v t ->
    match v with
    | Empty -> Seq.empty
    | Trap _t -> Seq.empty (* TODO do something useful with the trap *)
    | Retv v -> Seq.return (v, t)
    | Ret (St f) -> Seq.return (f t)
    | Bind (v, f) -> begin
      match v with
      | Choice _ when depth <= 0 -> Seq.empty
      | _ ->
        Seq.flat_map
          (fun (v, t) -> run (f v) t)
          (run_up_to ~depth:(depth - 1) v t)
    end
    | Choice cond -> List.to_seq (eval_choice cond t)
end

module type T =
  Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Sym_value.S

module type T_trap = Choice_monad_intf.Complete_with_trap
    with type thread := Thread.t
     and module V := Sym_value.S

let list = (module List : T)

let seq = (module Seq : T)

let explicit = (module Explicit : T)

let choices = [ list; seq; explicit ]

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Choice_monad_intf

type vbool = Symbolic_value.S.vbool

type vint32 = Symbolic_value.S.int32

exception Assertion of assertion * Thread.t

let check (sym_bool : vbool) (state : Thread.t) : bool =
  let (S (solver_module, solver)) = Thread.solver state in
  let pc = Thread.pc state in
  let no = Symbolic_value.S.Bool.not sym_bool in
  let no = Encoding.Expression.simplify no in
  match no with
  | Val (Bool no) -> not no
  | _ ->
    let check = no :: pc in
    Format.printf "CHECK:@.";
    List.iter (fun c -> print_endline (Encoding.Expression.to_string c)) check;
    let module Solver = (val solver_module) in
    let r = Solver.check solver check in
    let msg = if r then "KO" else "OK" in
    Format.printf "/CHECK %s@." msg;
    not r

let eval_choice (sym_bool : vbool) (state : Thread.t) : (bool * Thread.t) list =
  let (S (solver_module, s) as solver) = Thread.solver state in
  let pc = Thread.pc state in
  let memories = Thread.memories state in
  let tables = Thread.tables state in
  let globals = Thread.globals state in
  let sym_bool = Encoding.Expression.simplify sym_bool in
  match sym_bool with
  | Val (Bool b) -> [ (b, state) ]
  | Val (Num (I32 _)) -> assert false
  | _ -> (
    let no = Symbolic_value.S.Bool.not sym_bool in
    let module Solver = (val solver_module) in
    let sat_true = Solver.check s (sym_bool :: pc) in
    let sat_false = Solver.check s (no :: pc) in
    match (sat_true, sat_false) with
    | false, false -> []
    | true, false -> [ (true, state) ]
    | false, true -> [ (false, state) ]
    | true, true ->
      Format.printf "CHOICE: %s@." (Encoding.Expression.to_string sym_bool);
      let state1 : Thread.t =
        { solver
        ; pc = sym_bool :: pc
        ; memories = Symbolic_memory.clone memories
        ; tables = Symbolic_table.clone tables
        ; globals = Symbolic_global.clone globals
        }
      in
      let state2 : Thread.t =
        { solver
        ; pc = no :: pc
        ; memories = Symbolic_memory.clone memories
        ; tables = Symbolic_table.clone tables
        ; globals = Symbolic_global.clone globals
        }
      in
      [ (true, state1); (false, state2) ] )

let fix_symbol (e : Encoding.Expression.t) pc =
  let open Encoding in
  match e with
  | Symbol sym -> (pc, sym)
  | _ ->
    let sym = Symbol.mk_symbol `I32Type "choice_i32" in
    let assign = Expression.Relop (I32 Eq, Symbol sym, e) in
    (assign :: pc, sym)

let clone_if_needed ~orig_pc (cases : (int32 * Thread.t) list) :
  (int32 * Thread.t) list =
  match cases with
  | [] -> []
  | [ (i, state) ] -> [ (i, { state with pc = orig_pc }) ]
  | _ :: _ :: _ ->
    List.map
      (fun (i, state) ->
        let solver = Thread.solver state in
        let pc = Thread.pc state in
        let memories = Thread.memories state in
        let tables = Thread.tables state in
        let globals = Thread.globals state in
        let state : Thread.t =
          { solver
          ; pc
          ; memories = Symbolic_memory.clone memories
          ; tables = Symbolic_table.clone tables
          ; globals = Symbolic_global.clone globals
          }
        in
        (i, state) )
      cases

let not_value sym value =
  Encoding.Expression.Relop (I32 Ne, Symbol sym, Val (Num (I32 value)))

let eval_choice_i32 (sym_int : vint32) (state : Thread.t) :
  (int32 * Thread.t) list =
  let (S (solver_module, solver)) = Thread.solver state in
  let pc = Thread.pc state in
  let sym_int = Encoding.Expression.simplify sym_int in
  let orig_pc = pc in
  let pc, sym = fix_symbol sym_int pc in
  match sym_int with
  | Val (Num (I32 i)) -> [ (i, state) ]
  | _ ->
    let module Solver = (val solver_module) in
    let rec find_values values =
      let additionnal = List.map (not_value sym) values in
      if not (Solver.check solver (additionnal @ pc)) then []
      else begin
        let model = Solver.model ~symbols:[ sym ] solver in
        match model with
        | None -> assert false (* ? *)
        | Some model -> (
          let desc = Encoding.Model.to_string model in
          Format.printf "Model:@.%s@." desc;
          let v = Encoding.Model.evaluate model sym in
          match v with
          | None -> assert false (* ? *)
          | Some (Num (I32 i) as v) -> begin
            let cond = Encoding.Expression.Relop (I32 Eq, Symbol sym, Val v) in
            let case = (i, { state with pc = cond :: pc }) in
            case :: find_values (i :: values)
          end
          | Some _ -> assert false )
      end
    in
    let cases = find_values [] in
    clone_if_needed ~orig_pc cases

module List = struct
  type vbool = Symbolic_value.S.vbool

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

  let select_i32 (i : Symbolic_value.S.int32) : int32 t = eval_choice_i32 i

  let trap : Trap.t -> 'a t = function
    | Unreachable -> fun _ -> []
    | _ -> assert false

  (* raise (Types.Trap "out of bounds memory access") *)

  let with_thread (f : thread -> 'b) : 'b t = fun t -> [ (f t, t) ]

  let add_pc (c : Symbolic_value.S.vbool) : unit t =
   fun t ->
    match c with
    | Val (Bool b) -> if b then [ ((), t) ] else []
    | _ -> [ ((), { t with pc = c :: t.pc }) ]

  let assertion c t =
    if check c t then [ ((), t) ]
    else raise (Assertion (Encoding.Expression.to_string c, t))

  let run (v : 'a t) (thread : thread) = List.to_seq (v thread)
end

module Seq = struct
  module List = Stdlib.List

  type vbool = Symbolic_value.S.vbool

  type thread = Thread.t

  type 'a t = thread -> ('a * thread) Seq.t

  let return (v : 'a) : 'a t = fun t -> Seq.return (v, t)

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let seq = v t in
    Seq.flat_map (fun (e, t) -> f e t) seq

  let select (sym_bool : vbool) : bool t =
   fun state -> List.to_seq (eval_choice sym_bool state)

  let select_i32 (i : Symbolic_value.S.int32) : int32 t =
   fun state -> List.to_seq (eval_choice_i32 i state)

  let trap : Trap.t -> 'a t = function
    | Unreachable -> fun _ -> Seq.empty
    | _ -> assert false

  let assertion c t =
    if check c t then Seq.return ((), t)
    else raise (Assertion (Encoding.Expression.to_string c, t))

  (* raise (Types.Trap "out of bounds memory access") *)

  let with_thread (f : thread -> 'b) : 'b t = fun t -> Seq.return (f t, t)

  let add_pc (c : Symbolic_value.S.vbool) : unit t =
   fun t ->
    match c with
    | Val (Bool b) -> if b then Seq.return ((), t) else Seq.empty
    | _ -> Seq.return ((), { t with pc = c :: t.pc })

  let run (v : 'a t) (thread : thread) = v thread
end

module Explicit = struct
  module List = Stdlib.List
  module Seq = Stdlib.Seq

  type vbool = Symbolic_value.S.vbool

  type thread = Thread.t

  type 'a st = St of (thread -> 'a * thread) [@@unboxed]

  type 'a t =
    | Empty : 'a t
    | Ret : 'a st -> 'a t
    | Retv : 'a -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Assert : vbool -> unit t
    | Choice : vbool -> bool t
    | Choice_i32 : vint32 -> int32 t
    | Trap : Trap.t -> 'a t

  let return (v : 'a) : 'a t = Retv v [@@inline]

  let bind : type a b. a t -> (a -> b t) -> b t =
   fun v f ->
    match v with
    | Empty -> Empty
    | Trap t -> Trap t
    | Retv v -> f v
    | Assert _ | Ret _ | Choice _ | Choice_i32 _ -> Bind (v, f)
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

  let select_i32 (i : Symbolic_value.S.int32) : int32 t =
    match i with Val (Num (I32 v)) -> Retv v | _ -> Choice_i32 i

  let trap : Trap.t -> 'a t = fun t -> Trap t

  let with_thread (f : thread -> 'b) : 'b t = Ret (St (fun t -> (f t, t)))
  [@@inline]

  let add_pc (c : Symbolic_value.S.vbool) : unit t =
    match c with
    | Val (Bool b) -> if b then Retv () else Empty
    | _ -> Ret (St (fun t -> ((), { t with pc = c :: t.pc })))
  [@@inline]

  let assertion c : unit t = Assert c

  let rec run : type a. a t -> thread -> (a * thread) Seq.t =
   fun v t ->
    match v with
    | Empty -> Seq.empty
    | Trap _t -> Seq.empty (* TODO do something useful with the trap *)
    | Retv v -> Seq.return (v, t)
    | Ret (St f) -> Seq.return (f t)
    | Bind (v, f) -> Seq.flat_map (fun (v, t) -> run (f v) t) (run v t)
    | Assert c ->
      if check c t then Seq.return ((), t)
      else raise (Assertion (Encoding.Expression.to_string c, t))
    | Choice cond -> List.to_seq (eval_choice cond t)
    | Choice_i32 i -> List.to_seq (eval_choice_i32 i t)

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
          | EAssert f -> Seq.return (EAssert f, t)
          | ETrap tr -> Seq.return (ETrap tr, t)
          | EVal v -> run_and_trap (f v) t )
        (run_and_trap v t)
    | Assert c ->
      if check c t then Seq.return (EVal (), t)
      else Seq.return (EAssert (Encoding.Expression.to_string c), t)
    | Choice cond ->
      List.to_seq (List.map (fun (v, t) -> (EVal v, t)) (eval_choice cond t))
    | Choice_i32 i ->
      List.to_seq (List.map (fun (v, t) -> (EVal v, t)) (eval_choice_i32 i t))

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
    | Assert c ->
      if check c t then Seq.return ((), t)
      else raise (Assertion (Encoding.Expression.to_string c, t))
    | Choice cond -> List.to_seq (eval_choice cond t)
    | Choice_i32 i -> List.to_seq (eval_choice_i32 i t)

  type 'a cont = { k : thread -> 'a -> unit } [@@unboxed]

  type hold = H : thread * 'a t * 'a cont -> hold

  module WQ = struct
    type 'a t =
      { mutex : Mutex.t
      ; cond : Condition.t
      ; queue : 'a Queue.t
      ; mutable producers : int
      }

    let take_as_producer q =
      Mutex.lock q.mutex;
      q.producers <- q.producers - 1;
      (* Format.printf "TAKE COUNT %i@." q.producers; *)
      let r =
        try
          while Queue.is_empty q.queue do
            if q.producers = 0 then raise Exit;
            Condition.wait q.cond q.mutex
          done;
          let v = Queue.pop q.queue in
          q.producers <- q.producers + 1;
          Some v
        with Exit ->
          (* Format.printf "@.@.TAKE EXIT@.@."; *)
          Condition.broadcast q.cond;
          None
      in
      Mutex.unlock q.mutex;
      r

    let take_as_consumer q =
      Mutex.lock q.mutex;
      let r =
        try
          while Queue.is_empty q.queue do
            if q.producers = 0 then raise Exit;
            Condition.wait q.cond q.mutex
          done;
          let v = Queue.pop q.queue in
          Some v
        with Exit -> None
      in
      Mutex.unlock q.mutex;
      r

    let rec read_as_seq (q : 'a t) : 'a Seq.t =
     fun () ->
      match take_as_consumer q with
      | None -> Nil
      | Some v -> Cons (v, read_as_seq q)

    let produce (q : 'a t) (f : 'a -> unit) =
      let rec loop () =
        match take_as_producer q with
        | None -> ()
        | Some v ->
          f v;
          loop ()
      in
      Mutex.lock q.mutex;
      q.producers <- q.producers + 1;
      Mutex.unlock q.mutex;
      loop ()

    let push v q =
      Mutex.lock q.mutex;
      let was_empty = Queue.is_empty q.queue in
      Queue.push v q.queue;
      if was_empty then Condition.broadcast q.cond;
      Mutex.unlock q.mutex

    let with_produce q f =
      Mutex.lock q.mutex;
      q.producers <- q.producers + 1;
      Mutex.unlock q.mutex;
      f ();
      Mutex.lock q.mutex;
      q.producers <- q.producers - 1;
      if q.producers = 0 then Condition.broadcast q.cond;
      Mutex.unlock q.mutex

    let init () =
      { mutex = Mutex.create ()
      ; cond = Condition.create ()
      ; queue = Queue.create ()
      ; producers = 0
      }
  end

  module MT = struct
    type 'a global_state =
      { w : hold WQ.t (* work *)
      ; r : ('a eval * thread) WQ.t (* results *)
      }

    and 'a local_state =
      { solver : Thread.solver
      ; mutable next : hold option
      ; global : 'a global_state
      }

    and e_local_state = E_st : 'a local_state -> e_local_state [@@unboxed]

    and 'a cont = { k : thread -> e_local_state -> 'a -> unit } [@@unboxed]

    and hold = H : thread * 'a t * 'a cont -> hold

    let local_push st v =
      match st.next with
      | None -> st.next <- Some v
      | Some _ -> WQ.push v st.global.w

    let rec step : type v. thread -> v t -> v cont -> _ -> unit =
     fun thread t cont st ->
      match t with
      | Empty -> ()
      | Retv v -> cont.k thread (E_st st) v
      | Ret (St f) ->
        let v, thread = f thread in
        cont.k thread (E_st st) v
      | Trap t -> WQ.push (ETrap t, thread) st.global.r
      | Assert c ->
        if check c { thread with solver = st.solver } then
          cont.k thread (E_st st) ()
        else
          let assertion = Encoding.Expression.to_string c in
          WQ.push (EAssert assertion, thread) st.global.r
      | Bind (v, f) ->
        let k thread (E_st st) v =
          let r = f v in
          local_push st (H (thread, r, cont))
        in
        step thread v { k } st
      | Choice cond ->
        let cases = eval_choice cond { thread with solver = st.solver } in
        List.iter (fun (case, thread) -> cont.k thread (E_st st) case) cases
      | Choice_i32 i ->
        let cases = eval_choice_i32 i { thread with solver = st.solver } in
        List.iter (fun (case, thread) -> cont.k thread (E_st st) case) cases

    let init_global () =
      let w = WQ.init () in
      let r = WQ.init () in
      { w; r }

    let push_first_work g thread t =
      let k thread _st v = WQ.push (EVal v, thread) g.r in
      WQ.push (H (thread, t, { k })) g.w

    let spawn_producer i global =
      let module Mapping = Encoding.Z3_mappings.Fresh.Make () in
      let module Batch = Encoding.Batch.Make (Mapping) in
      let solver = Batch.create () in
      let solver : Thread.solver = S ((module Batch), solver) in
      let st = { solver; next = None; global } in
      let rec producer (H (thread, t, cont)) =
        let thread = { thread with solver } in
        step thread t cont st;
        match st.next with
        | Some h ->
          st.next <- None;
          producer h
        | None -> ()
      in
      Domain.spawn (fun () ->
        WQ.with_produce global.r (fun () ->
          WQ.produce global.w producer;
          ignore i (* Format.printf "@.@.PRODUCER END %i@.@." i; *) ) )

    let worker_threads_count = 4

    let rec loop_and_do (s : 'a Seq.t) f : 'a Seq.t =
     fun () ->
      match s () with
      | Cons (s, t) -> Cons (s, loop_and_do t f)
      | Nil ->
        (* Format.printf "@.@.End of stream@.@."; *)
        f ();
        Nil

    let run_and_trap : type a. a t -> thread -> (a eval * thread) Seq.t =
     fun (t : a t) (thread : thread) ->
      let global = init_global () in
      push_first_work global thread t;
      let producers =
        List.init worker_threads_count (fun i -> spawn_producer i global)
      in
      loop_and_do (WQ.read_as_seq global.r) (fun () ->
        List.iter Domain.join producers )
  end

  let rec step : type v. thread -> v t -> v cont -> _ -> _ -> unit =
   fun thread t cont q qt ->
    match t with
    | Empty -> ()
    | Retv v -> cont.k thread v
    | Ret (St f) ->
      let v, thread = f thread in
      cont.k thread v
    | Trap t -> Queue.push (ETrap t, thread) qt
    | Assert c ->
      if check c thread then cont.k thread ()
      else
        let assertion = Encoding.Expression.to_string c in
        Queue.push (EAssert assertion, thread) qt
    | Bind (v, f) ->
      let k thread v =
        let r = f v in
        Queue.push (H (thread, r, cont)) q
      in
      step thread v { k } q qt
    | Choice cond ->
      let cases = eval_choice cond thread in
      List.iter (fun (case, thread) -> cont.k thread case) cases
    | Choice_i32 i ->
      let cases = eval_choice_i32 i thread in
      List.iter (fun (case, thread) -> cont.k thread case) cases

  let init thread t =
    let q = Queue.create () in
    let qt = Queue.create () in
    let k thread v = Queue.push (EVal v, thread) qt in
    Queue.push (H (thread, t, { k })) q;
    (q, qt)

  let rec sequify q qt : 'a Seq.t =
   fun () ->
    while Queue.is_empty qt && not (Queue.is_empty q) do
      let (H (thread, t, cont)) = Queue.pop q in
      step thread t cont q qt
    done;
    if Queue.is_empty qt then Nil
    else
      let first = Queue.pop qt in
      let head = Queue.to_seq qt in
      let tail = sequify q qt in
      Cons (first, Seq.append head tail)

  let () = ignore run_and_trap

  let run_and_trap : type a. a t -> thread -> (a eval * thread) Seq.t =
   fun t thread ->
    let q, qt = init thread t in
    sequify q qt

  let () = ignore MT.run_and_trap

  let () = ignore run_and_trap

  let run_and_trap = MT.run_and_trap
end

module type T =
  Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Symbolic_value.S

module type T_trap =
  Choice_monad_intf.Complete_with_trap
    with type thread := Thread.t
     and module V := Symbolic_value.S

let list = (module List : T)

let seq = (module Seq : T)

let explicit = (module Explicit : T)

let choices = [ list; seq; explicit ]

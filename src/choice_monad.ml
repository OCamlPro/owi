(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Choice_monad_intf

type vbool = Symbolic_value.S.vbool

type thread = Thread.t

type vint32 = Symbolic_value.S.int32

exception Assertion of Encoding.Expression.t * Thread.t

let check (sym_bool : vbool) (state : Thread.t) : bool =
  let (S (solver_module, solver)) = Thread.solver state in
  let pc = Thread.pc state in
  let no = Symbolic_value.S.Bool.not sym_bool in
  let no = Encoding.Expression.simplify no in
  match no with
  | Val (Bool no) -> not no
  | _ ->
    let check = no :: pc in
    Format.printf "CHECK:@.%a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_newline
         Encoding.Expression.pp )
      check;
    let module Solver = (val solver_module) in
    let r = Solver.check solver check in
    let msg = if r then "KO" else "OK" in
    Format.printf "@./CHECK %s@." msg;
    not r

(* TODO: make this a CLI flag ? *)
let print_choice = false

module Make (M : sig
  type 'a t

  val return : 'a -> 'a t

  val empty : 'a t

  val length : 'a t -> int

  val hd : 'a t -> 'a

  val cons : 'a -> 'a t -> 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t

  val flat_map : ('a -> 'b t) -> 'a t -> 'b t

  val to_list : 'a t -> 'a list

  val to_seq : 'a t -> 'a Seq.t
end) =
struct
  include M

  type 'a t = thread -> ('a * thread) M.t

  let select b ({ Thread.pc; solver = S (solver_module, s); _ } as state) =
    match Encoding.Expression.simplify b with
    | Val (Bool b) -> M.return (b, state)
    | Val (Num (I32 _)) -> assert false
    | e -> (
      let module Solver = (val solver_module) in
      let with_e = e :: pc in
      let with_not_e = Symbolic_value.S.Bool.not e :: pc in
      let sat_true = Solver.check s with_e in
      let sat_false = Solver.check s with_not_e in
      match (sat_true, sat_false) with
      | false, false -> M.empty
      | true, false | false, true -> M.return (sat_true, state)
      | true, true ->
        if print_choice then
          Format.printf "CHOICE: %a@." Encoding.Expression.pp e;
        let state1 = Thread.clone { state with pc = with_e } in
        let state2 = Thread.clone { state with pc = with_not_e } in
        M.cons (true, state1) (M.return (false, state2)) )

  let fix_symbol (e : Encoding.Expression.t) pc =
    let open Encoding in
    match e with
    | Symbol sym -> (pc, sym)
    | _ ->
      let sym = Symbol.mk_symbol `I32Type "choice_i32" in
      let assign = Expression.Relop (I32 Eq, Symbol sym, e) in
      (assign :: pc, sym)

  let clone_if_needed ~orig_pc cases =
    match M.length cases with
    | 0 -> cases
    | 1 ->
      let i, state = M.hd cases in
      M.return (i, { state with Thread.pc = orig_pc })
    | _n ->
      M.map
        (fun (i, state) ->
          let state = Thread.clone state in
          (i, state) )
        cases

  let not_value sym value =
    Encoding.Expression.Relop (I32 Ne, Symbol sym, Val (Num (I32 value)))

  let select_i32 (sym_int : vint32) (state : Thread.t) =
    let (S (solver_module, solver)) = Thread.solver state in
    let pc = Thread.pc state in
    let sym_int = Encoding.Expression.simplify sym_int in
    let orig_pc = pc in
    let pc, sym = fix_symbol sym_int pc in
    match sym_int with
    | Val (Num (I32 i)) -> M.return (i, state)
    | _ ->
      let module Solver = (val solver_module) in
      let rec find_values values =
        let additionnal = M.to_list @@ M.map (not_value sym) values in
        if not (Solver.check solver (additionnal @ pc)) then M.empty
        else begin
          let model = Solver.model ~symbols:[ sym ] solver in
          match model with
          | None -> assert false (* ? *)
          | Some model -> (
            Format.printf "Model:@.%a@." Encoding.Model.pp model;
            let v = Encoding.Model.evaluate model sym in
            match v with
            | None -> assert false (* ? *)
            | Some (Num (I32 i) as v) -> begin
              let cond =
                Encoding.Expression.Relop (I32 Eq, Symbol sym, Val v)
              in
              let pc = cond :: pc in
              let case = (i, { state with pc }) in
              M.cons case (find_values (M.cons i values))
            end
            | Some _ -> assert false )
        end
      in
      let cases = find_values M.empty in
      clone_if_needed ~orig_pc cases

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t -> M.flat_map (fun (r, t) -> (f r) t) (v t)

  let trap = function Trap.Unreachable -> fun _ -> M.empty | _ -> assert false

  let with_thread f t = M.return (f t, t)

  let add_pc c t =
    match c with
    | Encoding.Expression.Val (Bool b) ->
      if b then M.return ((), t) else M.empty
    | _ ->
      let pc = c :: t.Thread.pc in
      let t = { t with pc } in
      M.return ((), t)

  let assertion c (t : Thread.t) =
    if check c t then M.return ((), t)
    else
      let no = Symbolic_value.S.Bool.not c in
      let t = { t with pc = no :: t.pc } in
      raise (Assertion (c, t))

  let run v t = v t |> M.to_seq

  let return v t = M.return (v, t)
end
[@@inline]

module CList = Make (struct
  include List

  let return v = [ v ]

  let empty = []

  let flat_map = List.concat_map

  let to_list = Fun.id
end)

module CSeq = Make (struct
  include Seq

  let hd s = Seq.uncons s |> Option.get |> fst

  let to_list = List.of_seq

  let to_seq = Fun.id
end)

module Common_sausage = struct
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
end

module Explicit = struct
  include Common_sausage

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
      else
        let no = Symbolic_value.S.Bool.not c in
        let t = { t with pc = no :: t.pc } in
        raise (Assertion (c, t))
    | Choice cond -> CSeq.select cond t
    | Choice_i32 i -> CSeq.select_i32 i t

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
      else
        let no = Symbolic_value.S.Bool.not c in
        let t = { t with pc = no :: t.pc } in
        Seq.return (EAssert c, t)
    | Choice cond -> Seq.map (fun (v, t) -> (EVal v, t)) (CSeq.select cond t)
    | Choice_i32 i -> Seq.map (fun (v, t) -> (EVal v, t)) (CSeq.select_i32 i t)

  let run_and_trap ~workers:_ = run_and_trap

  let () = ignore run_and_trap

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
      else
        let no = Symbolic_value.S.Bool.not c in
        let t = { t with pc = no :: t.pc } in
        raise (Assertion (c, t))
    | Choice cond -> CSeq.select cond t
    | Choice_i32 i -> CSeq.select_i32 i t

  type 'a cont = { k : thread -> 'a -> unit } [@@unboxed]

  type hold = H : thread * 'a t * 'a cont -> hold

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
        let no = Symbolic_value.S.Bool.not c in
        let thread = { thread with pc = no :: thread.pc } in
        Queue.push (EAssert c, thread) qt
    | Bind (v, f) ->
      let k thread v =
        let r = f v in
        Queue.push (H (thread, r, cont)) q
      in
      step thread v { k } q qt
    | Choice cond ->
      let cases = CList.select cond thread in
      List.iter (fun (case, thread) -> cont.k thread case) cases
    | Choice_i32 i ->
      let cases = CList.select_i32 i thread in
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

  let run_and_trap :
    type a. workers:int -> a t -> thread -> (a eval * thread) Seq.t =
   fun ~workers:_ t thread ->
    let q, qt = init thread t in
    sequify q qt
end

module WQ = struct
  type 'a t =
    { mutex : Mutex.t
    ; cond : Condition.t
    ; queue : 'a Queue.t
    ; mutable producers : int
    ; mutable failed : bool
    }

  let take_as_producer q =
    Mutex.lock q.mutex;
    q.producers <- q.producers - 1;
    (* Format.printf "TAKE COUNT %i@." q.producers; *)
    let r =
      try
        while Queue.is_empty q.queue do
          if q.producers = 0 || q.failed then raise Exit;
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
          if q.producers = 0 || q.failed then raise Exit;
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

  let fail q =
    Mutex.lock q.mutex;
    q.failed <- true;
    Condition.broadcast q.cond;
    Mutex.unlock q.mutex

  let with_produce q f ~started =
    Mutex.lock q.mutex;
    q.producers <- q.producers + 1;
    Mutex.unlock q.mutex;
    started ();
    match f () with
    | () ->
      Mutex.lock q.mutex;
      q.producers <- q.producers - 1;
      if q.producers = 0 then Condition.broadcast q.cond;
      Mutex.unlock q.mutex
    | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      fail q;
      Printexc.raise_with_backtrace e bt

  let init () =
    { mutex = Mutex.create ()
    ; cond = Condition.create ()
    ; queue = Queue.create ()
    ; producers = 0
    ; failed = false
    }
end

module Counter = struct
  type t =
    { mutable c : int
    ; mutex : Mutex.t
    ; cond : Condition.t
    }

  let incr t =
    Mutex.lock t.mutex;
    t.c <- t.c + 1;
    Condition.broadcast t.cond;
    Mutex.unlock t.mutex

  let wait_n t n =
    Mutex.lock t.mutex;
    while t.c < n do
      Condition.wait t.cond t.mutex
    done;
    Mutex.unlock t.mutex

  let create () = { c = 0; mutex = Mutex.create (); cond = Condition.create () }
end

module MT = struct
  include Common_sausage

  type 'a global_state =
    { w : hold WQ.t (* work *)
    ; r : ('a eval * thread) WQ.t (* results *)
    ; start_counter : Counter.t
    }

  and 'a local_state =
    { solver : Thread.solver
    ; mutable next : hold option
    ; global : 'a global_state
    }

  and e_local_state = E_st : 'a local_state -> e_local_state [@@unboxed]

  and 'a cont = { k : thread -> e_local_state -> 'a -> unit } [@@unboxed]

  and hold = H : thread * 'a Explicit.t * 'a cont -> hold

  let local_push st v =
    match st.next with
    | None -> st.next <- Some v
    | Some _ -> WQ.push v st.global.w

  let rec step : type v. thread -> v Explicit.t -> v cont -> _ -> unit =
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
        let no = Symbolic_value.S.Bool.not c in
        let thread = { thread with pc = no :: thread.pc } in
        WQ.push (EAssert c, thread) st.global.r
    | Bind (v, f) ->
      let k thread (E_st st) v =
        let r = f v in
        local_push st (H (thread, r, cont))
      in
      step thread v { k } st
    | Choice cond ->
      let cases = CList.select cond { thread with solver = st.solver } in
      List.iter (fun (case, thread) -> cont.k thread (E_st st) case) cases
    | Choice_i32 i ->
      let cases = CList.select_i32 i { thread with solver = st.solver } in
      List.iter (fun (case, thread) -> cont.k thread (E_st st) case) cases

  let init_global () =
    let w = WQ.init () in
    let r = WQ.init () in
    let start_counter = Counter.create () in
    { w; r; start_counter }

  let push_first_work g thread t =
    let k thread _st v = WQ.push (EVal v, thread) g.r in
    WQ.push (H (thread, t, { k })) g.w

  let spawn_producer _i global =
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
        WQ.with_produce global.r
          ~started:(fun () -> Counter.incr global.start_counter)
          (fun () -> WQ.produce global.w producer) )

  let rec loop_and_do (s : 'a Seq.t) f : 'a Seq.t =
   fun () ->
    match s () with
    | Cons (s, t) -> Cons (s, loop_and_do t f)
    | Nil ->
      f ();
      Nil

  let run = Explicit.run

  let run_and_trap :
    type a. workers:int -> a Explicit.t -> thread -> (a eval * thread) Seq.t =
   fun ~workers (t : a Explicit.t) (thread : thread) ->
    let global = init_global () in
    push_first_work global thread t;
    let producers = List.init workers (fun i -> spawn_producer i global) in
    Counter.wait_n global.start_counter workers;
    loop_and_do (WQ.read_as_seq global.r) (fun () ->
        List.iter Domain.join producers )
end

module type T =
  Choice_monad_intf.Complete
    with type thread := Thread.t
     and module V := Symbolic_value.S

module type T_trap =
  Choice_monad_intf.Complete_with_trap
    with type thread := Thread.t
     and module V := Symbolic_value.S

let list = (module CList : T)

let seq = (module CSeq : T)

let explicit = (module Explicit : T)

let multicore = (module MT : T)

let choices = [ list; seq; explicit; multicore ]

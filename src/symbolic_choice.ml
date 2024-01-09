(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Encoding
open Choice_intf
open Symbolic_value

exception Assertion of Expr.t * Thread.t

let check sym_bool thread =
  let (S (solver_module, solver)) = Thread.solver thread in
  let pc = Thread.pc thread in
  let no = S.Bool.not sym_bool in
  let no = Expr.simplify no in
  match no.e with
  | Val True -> false
  | Val False -> true
  | _ ->
    let check = no :: pc in
    (*
    Format.pp_std "CHECK:@.%a"
      (Format.pp_list ~pp_sep:Format.pp_newline Expr.pp)
      check;
    *)
    let module Solver = (val solver_module) in
    let r = Solver.check solver check in
    (*
    let msg = if r then "KO" else "OK" in
    Format.pp_std "@./CHECK %s@." msg;
    *)
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

  val to_list : 'a t -> 'a list
end) : sig
  val select : Encoding.Expr.t -> Thread.t -> (bool * Thread.t) M.t

  val select_i32 : S.int32 -> Thread.t -> (int32 * Thread.t) M.t
end = struct
  include M

  let select b ({ Thread.pc; solver = S (solver_module, s); _ } as thread) =
    let v = Expr.simplify b in
    match v.e with
    | Val True -> M.return (true, thread)
    | Val False -> M.return (false, thread)
    | Val (Num (I32 _)) -> assert false
    | _ -> (
      let module Solver = (val solver_module) in
      let with_v = v :: pc in
      let with_not_v = S.Bool.not v :: pc in
      let sat_true = Solver.check s with_v in
      let sat_false = Solver.check s with_not_v in
      match (sat_true, sat_false) with
      | false, false -> M.empty
      | true, false | false, true -> M.return (sat_true, thread)
      | true, true ->
        if print_choice then Format.pp_std "CHOICE: %a@." Expr.pp v;
        let thread1 = Thread.clone { thread with pc = with_v } in
        let thread2 = Thread.clone { thread with pc = with_not_v } in
        M.cons (true, thread1) (M.return (false, thread2)) )

  let fix_symbol (e : Expr.t) pc =
    match e.e with
    | Symbol sym -> (pc, sym)
    | _ ->
      let sym = Symbol.("choice_i32" @: Ty_bitv S32) in
      let assign = Expr.(Relop (Eq, mk_symbol sym, e) @: Ty_bitv S32) in
      (assign :: pc, sym)

  let clone_if_needed ~orig_pc cases =
    match M.length cases with
    | 0 -> cases
    | 1 ->
      let i, thread = M.hd cases in
      M.return (i, { thread with Thread.pc = orig_pc })
    | _n ->
      M.map
        (fun (i, thread) ->
          let thread = Thread.clone thread in
          (i, thread) )
        cases

  let not_value sym value =
    Expr.(Relop (Ne, mk_symbol sym, S.const_i32 value) @: Ty_bitv S32)

  let select_i32 sym_int thread =
    let (S (solver_module, solver)) = Thread.solver thread in
    let pc = Thread.pc thread in
    let sym_int = Expr.simplify sym_int in
    let orig_pc = pc in
    let pc, symbol = fix_symbol sym_int pc in
    match sym_int.e with
    | Val (Num (I32 i)) -> M.return (i, thread)
    | _ ->
      let module Solver = (val solver_module) in
      let rec find_values values =
        let additionnal = M.to_list @@ M.map (not_value symbol) values in
        if not (Solver.check solver (additionnal @ pc)) then M.empty
        else begin
          let model = Solver.model ~symbols:[ symbol ] solver in
          match model with
          | None -> assert false (* ? *)
          | Some model -> (
            (*
            Format.pp_std "Model:@.%a@." Model.pp model;
            *)
            let v = Model.evaluate model symbol in
            match v with
            | None -> assert false (* ? *)
            | Some (Num (I32 i)) -> begin
              let cond = Expr.Bitv.I32.(Expr.mk_symbol symbol = v i) in
              let pc = cond :: pc in
              let case = (i, { thread with pc }) in
              M.cons case (find_values (M.cons i values))
            end
            | Some _ -> assert false )
        end
      in
      let cases = find_values M.empty in
      clone_if_needed ~orig_pc cases
end
[@@inline]

module CList = Make (struct
  include List

  let return v = [ v ]

  let empty = []

  let to_list = Fun.id
end)

module CSeq = Make (struct
  include Seq

  let hd s = Seq.uncons s |> Option.get |> fst

  let to_list = List.of_seq
end)

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
  type 'a st = St of (Thread.t -> 'a * Thread.t) [@@unboxed]

  type 'a t =
    | Empty : 'a t
    | Ret : 'a st -> 'a t
    | Retv : 'a -> 'a t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Assert : S.vbool -> unit t
    | Choice : S.vbool -> bool t
    | Choice_i32 : S.int32 -> int32 t
    | Trap : Trap.t -> 'a t

  let return v = Retv v [@@inline]

  let bind : type a b. a t -> (a -> b t) -> b t =
   fun v f ->
    match v with
    | Empty -> Empty
    | Trap t -> Trap t
    | Retv v -> f v
    | Assert _ | Ret _ | Choice _ | Choice_i32 _ -> Bind (v, f)
    | Bind _ -> Bind (v, f)
  [@@inline]

  let ( let* ) = bind

  let select (cond : S.vbool) =
    match cond.e with
    | Val True -> Retv true
    | Val False -> Retv false
    | _ -> Choice cond
  [@@inline]

  let select_i32 (i : S.int32) =
    match i.e with Val (Num (I32 v)) -> Retv v | _ -> Choice_i32 i

  let trap t = Trap t

  let with_thread f = Ret (St (fun t -> (f t, t))) [@@inline]

  let add_pc (c : S.vbool) =
    match c.e with
    | Val True -> Retv ()
    | Val False -> Empty
    | _ -> Ret (St (fun t -> ((), { t with pc = c :: t.pc })))
  [@@inline]

  let assertion c = Assert c

  type 'a global_state =
    { w : hold WQ.t (* work *)
    ; r : ('a eval * Thread.t) WQ.t (* results *)
    ; start_counter : Counter.t
    }

  and 'a local_state =
    { solver : Thread.solver
    ; mutable next : hold option
    ; global : 'a global_state
    }

  and e_local_state = E_st : 'a local_state -> e_local_state [@@unboxed]

  and 'a cont = { k : Thread.t -> e_local_state -> 'a -> unit } [@@unboxed]

  and hold = H : Thread.t * 'a t * 'a cont -> hold

  let local_push st v =
    match st.next with
    | None -> st.next <- Some v
    | Some _ -> WQ.push v st.global.w

  let rec step : type v. Thread.t -> v t -> v cont -> _ -> unit =
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
        let no = S.Bool.not c in
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

  let spawn_producer global _i =
    let module Mapping = Z3_mappings.Fresh.Make () in
    let module Batch = Solver.Batch (Mapping) in
    let solver = Batch.create ~logic:QF_BVFP () in
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
        try
          WQ.with_produce global.r
            ~started:(fun () -> Counter.incr global.start_counter)
            (fun () -> WQ.produce global.w producer)
        with e ->
          let bt = Printexc.get_raw_backtrace () in
          WQ.fail global.w;
          Printexc.raise_with_backtrace e bt )

  let rec loop_and_do (s : 'a Seq.t) f : 'a Seq.t =
   fun () ->
    match s () with
    | Cons (s, t) -> Cons (s, loop_and_do t f)
    | Nil ->
      f ();
      Nil

  let rec run : type a. a t -> Thread.t -> (a * Thread.t) Seq.t =
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
        let no = S.Bool.not c in
        let t = { t with pc = no :: t.pc } in
        raise (Assertion (c, t))
    | Choice cond -> CSeq.select cond t
    | Choice_i32 i -> CSeq.select_i32 i t

  let run_and_trap ~workers t thread =
    let global = init_global () in
    push_first_work global thread t;
    let producers = Array.init workers (spawn_producer global) in
    Counter.wait_n global.start_counter workers;
    loop_and_do (WQ.read_as_seq global.r) (fun () ->
        Array.iter Domain.join producers )
end

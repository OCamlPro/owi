(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021 Léo Andrès *)
(* Copyright © 2021 Pierre Chambart *)

open Solver
open Encoding
open Symbolic_value

exception Assertion of Expr.t * Thread.t

module Minimalist = struct
  type err =
    | Assert_fail
    | Trap of Trap.t

  type 'a t = M of (Thread.t -> solver -> ('a, err) Stdlib.Result.t * Thread.t)
  [@@unboxed]

  type 'a run_result = ('a, err) Stdlib.Result.t * Thread.t

  let return v = M (fun t _sol -> (Ok v, t))

  let run (M v) st s : _ run_result = v st s

  let bind v f =
    M
      (fun init_s sol ->
        let v_final, tmp_st = run v init_s sol in
        match v_final with
        | Ok v_final -> run (f v_final) tmp_st sol
        | Error _ as e -> (e, tmp_st) )

  let ( let* ) = bind

  let map v f =
    let* v in
    return (f v)

  let ( let+ ) = map

  let select (vb : vbool) =
    let v = Expr.simplify vb in
    match Expr.view v with
    | Val True -> return true
    | Val False -> return false
    | _ -> Format.kasprintf failwith "%a" Expr.pp v

  let select_i32 (i : int32) =
    let v = Expr.simplify i in
    match Expr.view v with Val (Num (I32 i)) -> return i | _ -> assert false

  let trap t = M (fun th _sol -> (Error (Trap t), th))

  let assertion (vb : vbool) =
    let v = Expr.simplify vb in
    match Expr.view v with
    | Val True -> return ()
    | Val False -> M (fun th _sol -> (Error Assert_fail, th))
    | _ -> assert false

  let with_thread f = M (fun st _sol -> (Ok (f st), st))

  let thread = M (fun st _sol -> (Ok st, st))

  let solver = M (fun st sol -> (Ok sol, st))

  let add_pc (_vb : vbool) = return ()

  let run ~workers:_ t thread = run t thread (fresh_solver ())
end

module WQ = struct
  type 'a t =
    { mutex : Mutex.t
    ; cond : Condition.t
    ; queue : 'a Queue.t
    ; mutable pledges : int
    ; mutable failed : bool
    }

  let take q pledge =
    Mutex.lock q.mutex;
    let r =
      try
        while Queue.is_empty q.queue do
          if q.pledges = 0 || q.failed then raise Exit;
          Condition.wait q.cond q.mutex
        done;
        let v = Queue.pop q.queue in
        if pledge then q.pledges <- q.pledges + 1;
        Some v
      with Exit ->
        Condition.broadcast q.cond;
        None
    in
    Mutex.unlock q.mutex;
    r

  let make_pledge q =
    Mutex.lock q.mutex;
    q.pledges <- q.pledges + 1;
    Mutex.unlock q.mutex

  let end_pledge q =
    Mutex.lock q.mutex;
    q.pledges <- q.pledges - 1;
    Condition.broadcast q.cond;
    Mutex.unlock q.mutex

  let rec read_as_seq (q : 'a t) ?(finalizer = Fun.const ()) : 'a Seq.t =
   fun () ->
    match take q false with
    | None ->
      finalizer ();
      Nil
    | Some v -> Cons (v, read_as_seq q ~finalizer)

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

  let init () =
    { mutex = Mutex.create ()
    ; cond = Condition.create ()
    ; queue = Queue.create ()
    ; pledges = 0
    ; failed = false
    }
end

module Multicore = struct
  (*
     Multicore is based on several layers of monad transformers defined here
     in submodules. The module as a whole is made to provide a monad to explore in parallel
     different possibilites, with a notion of priority.
  *)
  module Prio = struct
    (*
      Currently there is no real notion of priority. Future extensions adding it will ho here.
    *)
    type t = Default

    let default = Default
  end

  module CoreImpl : sig
    (*
      The core implementation of the monad. It is isolated in a module to restict its exposed interface
      and maintain its invariant. In particular, choose must guarantee that the Thread.t is cloned in each branch.
      Using functions defined here should be foolproof.
    *)
    type 'a t

    val return : 'a -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val map : 'a t -> ('a -> 'b) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val stop : 'a t

    val assertion_fail : Expr.t -> 'a t

    val trap : Trap.t -> 'a t

    val thread : Thread.t t

    val yield : unit t

    val solver : solver t

    val with_thread : (Thread.t -> 'a) -> 'a t

    val set_thread : Thread.t -> unit t

    val modify_thread : (Thread.t -> Thread.t) -> unit t

    (*
       Indicates a possible choice between two values. Thread duplication
       is already handled by choose and should not be done before by the caller.
    *)
    val choose : 'a t -> 'a t -> 'a t

    type 'a eval =
      | EVal of 'a
      | ETrap of Trap.t
      | EAssert of Encoding.Expr.t

    type 'a run_result = ('a eval * Thread.t) Seq.t

    val run : workers:int -> 'a t -> Thread.t -> 'a run_result
  end = struct
    module Schedulable = struct
      (*
        A monad representing computation that can be cooperatively scheduled and may need
        Worker Local Storage (WLS). Computations can yield, and fork (Choice).
      *)
      type ('a, 'wls) t = Sched of ('wls -> ('a, 'wls) status) [@@unboxed]

      and ('a, 'wls) status =
        | Now of 'a
        | Yield of Prio.t * ('a, 'wls) t
        | Choice of (('a, 'wls) status * ('a, 'wls) status)
        | Stop

      let run (Sched mxf) wls = mxf wls

      let return x : _ t = Sched (Fun.const (Now x))

      let return_status status = Sched (Fun.const status)

      let rec bind (mx : ('a, 'wls) t) (f : 'a -> ('b, 'wls) t) : _ t =
        let rec bind_status (x : _ status) (f : _ -> _ status) : _ status =
          match x with
          | Now x -> f x
          | Yield (prio, lx) ->
            Yield (prio, Sched (fun wls -> bind_status (run lx wls) f))
          | Choice (mx1, mx2) -> Choice (bind_status mx1 f, bind_status mx2 f)
          | Stop -> Stop
        in
        Sched
          (fun wls ->
            let argumented_f x = run (f x) wls in
            match run mx wls with
            | Yield (prio, mx) -> Yield (prio, bind mx f)
            | x -> bind_status x argumented_f )

      let ( let* ) = bind

      let map x f =
        let* x in
        return (f x)

      let ( let+ ) = map

      let yield prio = return_status (Yield (prio, Sched (Fun.const (Now ()))))

      let choose a b = Sched (fun wls -> Choice (run a wls, run b wls))

      let stop : ('a, 'b) t = return_status Stop

      let worker_local : ('a, 'a) t = Sched (fun wls -> Now wls)
    end

    module Scheduler = struct
      (*
        A scheduler for Schedulable values.
      *)
      type ('a, 'wls) work_queue = ('a, 'wls) Schedulable.t WQ.t

      type 'a res_queue = 'a WQ.t

      type ('a, 'wls) t =
        { work_queue : ('a, 'wls) work_queue
        ; res_writer : 'a res_queue
        }

      let init_scheduler () =
        let work_queue = WQ.init () in
        let res_writer = WQ.init () in
        { work_queue; res_writer }

      let add_init_task sched task = WQ.push task sched.work_queue

      let rec work wls sched =
        let rec handle_status (t : _ Schedulable.status) sched =
          match t with
          | Stop -> ()
          | Now x -> WQ.push x sched.res_writer
          | Yield (_prio, f) -> WQ.push f sched.work_queue
          | Choice (m1, m2) ->
            handle_status m1 sched;
            handle_status m2 sched
        in
        match WQ.take sched.work_queue true with
        | None -> ()
        | Some f -> begin
          handle_status (Schedulable.run f wls) sched;
          WQ.end_pledge sched.work_queue;
          work wls sched
        end

      let spawn_worker sched wls_init =
        WQ.make_pledge sched.res_writer;
        Domain.spawn (fun () ->
            let wls = wls_init () in
            try
              work wls sched;
              WQ.end_pledge sched.res_writer
            with e ->
              let bt = Printexc.get_raw_backtrace () in
              WQ.fail sched.work_queue;
              WQ.end_pledge sched.res_writer;
              Printexc.raise_with_backtrace e bt )
    end

    module State = struct
      (*
        Add a notion of State to the Schedulable monad
        ("Transformer without module functor" style)
      *)
      module M = Schedulable

      type 'a t = St of (Thread.t -> ('a * Thread.t, solver) M.t) [@@unboxed]

      let run (St mxf) st = mxf st

      let return x = St (fun st -> M.return (x, st))

      let lift x =
        let ( let+ ) = M.( let+ ) in
        St
          (fun st ->
            let+ x in
            (x, st) )

      let bind mx f =
        St
          (fun st ->
            let ( let* ) = M.( let* ) in
            let* x, new_st = run mx st in
            run (f x) new_st )

      let ( let* ) = bind

      let map x f =
        let* x in
        return (f x)

      let liftF2 f x y = St (fun st -> f (run x st) (run y st))

      let ( let+ ) = map

      let with_state f = St (fun st -> M.return (f st))

      let modify_state f = St (fun st -> M.return ((), f st))
    end

    module Eval = struct
      (*
        Add a notion of faillibility to the evaluation
        ("Transformer without module functor" style)
      *)
      module M = State

      type 'a eval =
        | EVal of 'a
        | ETrap of Trap.t
        | EAssert of Encoding.Expr.t

      type 'a t = 'a eval M.t

      let return x : _ t = M.return (EVal x)

      let lift x =
        let ( let+ ) = M.( let+ ) in
        let+ x in
        EVal x

      let bind (mx : _ t) f : _ t =
        let ( let* ) = M.( let* ) in
        let* mx in
        match mx with
        | EVal x -> f x
        | ETrap _ as mx -> M.return mx
        | EAssert _ as mx -> M.return mx

      let ( let* ) = bind

      let map mx f =
        let ( let+ ) = M.( let+ ) in
        let+ mx in
        match mx with
        | EVal x -> EVal (f x)
        | ETrap _ as mx -> mx
        | EAssert _ as mx -> mx

      let ( let+ ) = map
    end

    include Eval

    (*
       Here we define functions to seamlessly
       operate on the three monads layers
    *)

    let lift_schedulable (v : ('a, _) Schedulable.t) : 'a t =
      lift (State.lift v)

    let with_thread f = lift (State.with_state (fun st -> (f st, st)))

    let thread = with_thread Fun.id

    let modify_thread f = lift (State.modify_state f)

    let set_thread st = modify_thread (Fun.const st)

    let clone_thread = modify_thread Thread.clone

    let solver = lift_schedulable Schedulable.worker_local

    let choose a b =
      let a =
        let* () = clone_thread in
        a
      in
      let b =
        let* () = clone_thread in
        b
      in
      State.liftF2 Schedulable.choose a b

    let yield = lift_schedulable @@ Schedulable.yield Prio.default

    let stop = lift_schedulable Schedulable.stop

    type 'a run_result = ('a eval * Thread.t) Seq.t

    let run ~workers t thread =
      let open Scheduler in
      let sched = init_scheduler () in
      add_init_task sched (State.run t thread);
      let join_handles =
        Array.map
          (fun () -> spawn_worker sched fresh_solver)
          (Array.init workers (Fun.const ()))
      in
      WQ.read_as_seq sched.res_writer ~finalizer:(fun () ->
          Array.iter Domain.join join_handles )

    let trap t = State.return (ETrap t)

    let assertion_fail c = State.return (EAssert c)
  end

  (*
    We can now use CoreImpl only through its exposed signature which
    maintains all invariants.
  *)

  include CoreImpl

  let add_pc (c : vbool) =
    match Expr.view c with
    | Val True -> return ()
    | Val False -> stop
    | _ ->
      let* thread in
      let new_thread = { thread with pc = c :: thread.pc } in
      set_thread new_thread
  [@@inline]

  let add_breadcrumb crumb =
    modify_thread (fun t -> { t with breadcrumbs = crumb :: t.breadcrumbs })

  (*
    Yielding is currently done each time the solver is about to be called,
    in check_reachability and get_model.
  *)
  let check_reachability =
    let* () = yield in
    let* (S (solver_module, s)) = solver in
    let module Solver = (val solver_module) in
    let* thread in
    let sat = Solver.check s thread.pc in
    if sat then return () else stop

  let get_model symbol =
    let* () = yield in
    let* (S (solver_module, s)) = solver in
    let module Solver = (val solver_module) in
    let+ thread in
    let sat = Solver.check s thread.pc in
    if not sat then None
    else begin
      let model = Solver.model ~symbols:[ symbol ] s in
      match model with
      | None ->
        failwith "Unreachable: The problem is sat so a model should exist"
      | Some model -> begin
        match Model.evaluate model symbol with
        | None ->
          failwith
            "Unreachable: The model exists so this symbol should evaluate"
        | Some _ as v -> v
      end
    end

  let get_model_or_stop symbol =
    let* model = get_model symbol in
    match model with Some v -> return v | None -> stop

  let select (cond : Symbolic_value.vbool) =
    let v = Expr.simplify cond in
    match Expr.view v with
    | Val True -> return true
    | Val False -> return false
    | Val (Num (I32 _)) -> failwith "unreachable (type error)"
    | _ ->
      let true_branch =
        let* () = add_pc v in
        let* () = add_breadcrumb 1l in
        let+ () = check_reachability in
        true
      in
      let false_branch =
        let* () = add_pc (Symbolic_value.Bool.not v) in
        let* () = add_breadcrumb 0l in
        let+ () = check_reachability in
        false
      in
      choose true_branch false_branch
  [@@inline]

  let summary_symbol (e : Expr.t) =
    let* thread in
    match Expr.view e with
    | Symbol sym -> return (None, sym)
    | _ ->
      let choices = thread.choices in
      let symbol_name = Format.sprintf "choice_i32_%i" choices in
      let+ () = modify_thread (fun t -> { t with choices = choices + 1 }) in
      let sym = Symbol.(symbol_name @: Ty_bitv 32) in
      let assign = Expr.(make (Relop (Ty_bitv 32, Eq, mk_symbol sym, e))) in
      (Some assign, sym)

  let select_i32 (i : Symbolic_value.int32) =
    let sym_int = Expr.simplify i in
    match Expr.view sym_int with
    | Val (Num (I32 i)) -> return i
    | _ ->
      let* assign, symbol = summary_symbol sym_int in
      let* () =
        match assign with Some assign -> add_pc assign | None -> return ()
      in
      let rec generator () =
        let* possible_value = get_model_or_stop symbol in
        let i =
          match possible_value with
          | Num (I32 i) -> i
          | _ -> failwith "Unreachable: found symbol must be a value"
        in
        let this_value_cond = Expr.Bitv.I32.(Expr.mk_symbol symbol = v i) in
        let not_this_value_cond =
          (* != is **not** the physical equality here *)
          Expr.Bitv.I32.(Expr.mk_symbol symbol != v i)
        in
        let this_val_branch =
          let* () = add_breadcrumb i in
          let+ () = add_pc this_value_cond in
          i
        in
        let not_this_val_branch =
          let* () = add_pc not_this_value_cond in
          generator ()
        in
        choose this_val_branch not_this_val_branch
      in
      generator ()

  let assertion c =
    let* assertion_true = select c in
    if assertion_true then return () else assertion_fail c
end

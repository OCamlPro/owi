(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Symbolic_choice_intf

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

module CoreImpl = struct
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

    let run (Sched mxf : ('a, 'wls) t) (wls : 'wls) : ('a, 'wls) status =
      mxf wls

    let return x : _ t = Sched (Fun.const (Now x))

    let return_status status = Sched (Fun.const status)

    let rec bind (mx : ('a, 'wls) t) (f : 'a -> ('b, 'wls) t) : ('b, 'wls) t =
      Sched
        (fun wls ->
          let rec unfold_status (x : ('a, 'wls) status) : ('b, 'wls) status =
            match x with
            | Now x -> run (f x) wls
            | Yield (prio, lx) -> Yield (prio, bind lx f)
            | Choice (mx1, mx2) ->
              let mx1' = unfold_status mx1 in
              let mx2' = unfold_status mx2 in
              Choice (mx1', mx2')
            | Stop -> Stop
          in
          unfold_status (run mx wls) )

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
    type ('a, 'wls) work_queue = ('a, 'wls) Schedulable.t Wq.t

    type ('a, 'wls) t = { work_queue : ('a, 'wls) work_queue } [@@unboxed]

    let init_scheduler () =
      let work_queue = Wq.make () in
      { work_queue }

    let add_init_task sched task = Wq.push task sched.work_queue

    let work wls sched callback =
      let rec handle_status (t : _ Schedulable.status) write_back =
        match t with
        | Stop -> ()
        | Now x -> callback x
        | Yield (_prio, f) -> write_back f
        | Choice (m1, m2) ->
          handle_status m1 write_back;
          handle_status m2 write_back
      in
      Wq.work_while
        (fun f write_back -> handle_status (Schedulable.run f wls) write_back)
        sched.work_queue

    let spawn_worker sched wls_init callback callback_init callback_close =
      callback_init ();
      Domain.spawn (fun () ->
        Fun.protect
          ~finally:(fun () -> callback_close ())
          (fun () ->
            let wls = wls_init () in
            try work wls sched callback
            with e ->
              let bt = Printexc.get_raw_backtrace () in
              Wq.fail sched.work_queue;
              Printexc.raise_with_backtrace e bt ) )
  end

  module State = struct
    (*
        Add a notion of State to the Schedulable monad
        ("Transformer without module functor" style)
      *)
    module M = Schedulable

    type ('a, 's) t = St of ('s -> ('a * 's, Solver.t) M.t) [@@unboxed]

    let run (St mxf) st = mxf st

    let return x = St (fun st -> M.return (x, st))

    let lift (x : ('a, _) M.t) : ('a, 's) t =
      let ( let+ ) = M.( let+ ) in
      St
        (fun (st : 's) ->
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

    let project_state (project_and_backup : 'st1 -> 'st2 * 'backup) restore
      other =
      St
        (fun st ->
          let ( let+ ) = M.( let+ ) in
          let proj, backup = project_and_backup st in
          let+ res, new_state = run other proj in
          (res, restore backup new_state) )
  end

  module Eval = struct
    (*
        Add a notion of faillibility to the evaluation
        ("Transformer without module functor" style)
      *)
    module M = State

    type ('a, 's) t = ('a eval, 's) M.t

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

  module Make (Thread : Thread.S) : sig
    (*
      The core implementation of the monad. It is isolated in a module to restict its exposed interface
      and maintain its invariant. In particular, choose must guarantee that the Thread.t is cloned in each branch.
      Using functions defined here should be foolproof.
    *)

    type thread := Thread.t

    type 'a t = ('a, Thread.t) Eval.t

    val return : 'a -> 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val map : 'a t -> ('a -> 'b) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val assertion_fail : Smtml.Expr.t -> Smtml.Model.t -> 'a t

    val stop : 'a t

    val trap : Trap.t -> 'a t

    val thread : thread t

    val yield : unit t

    val solver : Solver.t t

    val with_thread : (thread -> 'a) -> 'a t

    val set_thread : thread -> unit t

    val modify_thread : (thread -> thread) -> unit t

    (*
       Indicates a possible choice between two values. Thread duplication
       is already handled by choose and should not be done before by the caller.
    *)
    val choose : 'a t -> 'a t -> 'a t

    type 'a run_result = ('a eval * thread) Seq.t

    val run :
         workers:int
      -> Smtml.Solver_type.t
      -> 'a t
      -> thread
      -> callback:('a eval * thread -> unit)
      -> callback_init:(unit -> unit)
      -> callback_end:(unit -> unit)
      -> unit Domain.t array
  end = struct
    include Eval

    type 'a t = ('a, Thread.t) Eval.t

    (*
       Here we define functions to seamlessly
       operate on the three monads layers
    *)

    let lift_schedulable (v : ('a, _) Schedulable.t) : 'a t =
      let v = State.lift v in
      lift v

    let with_thread (f : Thread.t -> 'a) : 'a t =
      let x = State.with_state (fun st -> (f st, st)) in
      lift x

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

    let run ~workers solver t thread ~callback ~callback_init ~callback_end =
      let open Scheduler in
      let sched = init_scheduler () in
      add_init_task sched (State.run t thread);
      Array.init workers (fun _i ->
        spawn_worker sched (Solver.fresh solver) callback callback_init
          callback_end )

    let trap t =
      let* thread in
      let* solver in
      let pc = Thread.pc thread in
      let symbols = Thread.symbols_set thread |> Option.some in
      let model = Solver.model solver ~symbols ~pc in
      State.return (ETrap (t, model))

    let assertion_fail c model = State.return (EAssert (c, model))
  end
end

(*
    We can now use CoreImpl only through its exposed signature which
    maintains all invariants.
  *)
module Make (Thread : Thread.S) = struct
  include CoreImpl.Make (Thread)

  let add_pc (c : Symbolic_value.vbool) =
    match Smtml.Expr.view c with
    | Val True -> return ()
    | Val False -> stop
    | _ ->
      let* thread in
      let new_thread = Thread.add_pc thread c in
      set_thread new_thread
  [@@inline]

  let add_breadcrumb crumb =
    modify_thread (fun t -> Thread.add_breadcrumb t crumb)

  let with_new_symbol ty f =
    let* thread in
    let n = Thread.symbols thread in
    let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_%d" n in
    let+ () =
      modify_thread (fun thread ->
        let thread = Thread.add_symbol thread sym in
        Thread.incr_symbols thread )
    in
    f sym

  (*
    Yielding is currently done each time the solver is about to be called,
    in check_reachability and get_model.
  *)
  let check_reachability =
    let* () = yield in
    let* thread in
    let* solver in
    let pc = Thread.pc thread in
    match Solver.check solver pc with
    | `Sat -> return ()
    | `Unsat | `Unknown -> stop

  let get_model_or_stop symbol =
    let* () = yield in
    let* solver in
    let+ thread in
    let pc = Thread.pc thread in
    match Solver.check solver pc with
    | `Unsat | `Unknown -> stop
    | `Sat -> begin
      let symbols = [ symbol ] |> Option.some in
      let model = Solver.model solver ~symbols ~pc in
      match Smtml.Model.evaluate model symbol with
      | None ->
        Fmt.failwith
          "Unreachable: The model exists so this symbol should evaluate"
      | Some v -> return v
    end

  let select_inner ~explore_first (cond : Symbolic_value.vbool) =
    let v = Smtml.Expr.simplify cond in
    match Smtml.Expr.view v with
    | Val True -> return true
    | Val False -> return false
    | Val (Num (I32 _)) -> Fmt.failwith "unreachable (type error)"
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
      if explore_first then choose true_branch false_branch
      else choose false_branch true_branch
  [@@inline]

  let select (cond : Symbolic_value.vbool) =
    select_inner cond ~explore_first:true
  [@@inline]

  let summary_symbol (e : Smtml.Expr.t) =
    let* thread in
    match Smtml.Expr.view e with
    | Symbol sym -> return (None, sym)
    | _ ->
      let num_symbols = Thread.symbols thread in
      let+ () = modify_thread Thread.incr_symbols in
      let sym_name = Fmt.str "choice_i32_%i" num_symbols in
      let sym_type = Smtml.Ty.Ty_bitv 32 in
      let sym = Smtml.Symbol.make sym_type sym_name in
      let assign = Smtml.Expr.(relop Ty_bool Eq (symbol sym) e) in
      (Some assign, sym)

  let select_i32 (i : Symbolic_value.int32) =
    let sym_int = Smtml.Expr.simplify i in
    match Smtml.Expr.view sym_int with
    | Val (Num (I32 i)) -> return i
    | _ ->
      let* assign, symbol = summary_symbol sym_int in
      let* () =
        match assign with Some assign -> add_pc assign | None -> return ()
      in
      let rec generator () =
        let* possible_value = get_model_or_stop symbol in
        let* possible_value in
        let i =
          match possible_value with
          | Smtml.Value.Num (I32 i) -> i
          | _ -> Fmt.failwith "Unreachable: found symbol must be a value"
        in
        let s = Smtml.Expr.symbol symbol in
        let this_value_cond =
          let open Smtml.Expr in
          Bitv.I32.(s = v i)
        in
        let not_this_value_cond =
          let open Smtml.Expr in
          (* != is **not** the physical inequality here *)
          Bitv.I32.(s != v i)
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
    let* assertion_true = select_inner c ~explore_first:false in
    if assertion_true then return ()
    else
      let* thread in
      let* solver in
      let symbols = Thread.symbols_set thread |> Option.some in
      let pc = Thread.pc thread in
      let model = Solver.model ~symbols ~pc solver in
      assertion_fail c model
end

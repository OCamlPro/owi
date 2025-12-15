(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

include Symbolic_choice_intf

(*
     Multicore is based on several layers of monad transformers defined here
     in submodules. The module as a whole is made to provide a monad to explore in parallel
     different possibilites, with a notion of priority.
  *)

module CoreImpl = struct
  module Schedulable = struct
    (*
        A monad representing computation that can be cooperatively scheduled and may need
        Worker Local Storage (WLS). Computations can yield, and fork (Choice).
      *)
    type ('a, 'wls) t = Sched of ('wls -> ('a, 'wls) status) [@@unboxed]

    and ('a, 'wls) status =
      | Now of 'a
      | Yield of Prio.source * ('a, 'wls) t
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

  module Scheduler (Work_datastructure : Prio.S) = struct
    (*
        A scheduler for Schedulable values.
      *)
    type ('a, 'wls) work_queue = ('a, 'wls) Schedulable.t Work_datastructure.t

    type ('a, 'wls) t = { work_queue : ('a, 'wls) work_queue } [@@unboxed]

    let init_scheduler () =
      let work_queue = Work_datastructure.make () in
      { work_queue }

    let add_init_task sched task =
      Work_datastructure.push task Prio.dummy sched.work_queue

    let work wls sched at_worker_value =
      let rec handle_status (t : _ Schedulable.status) write_back =
        match t with
        | Stop -> ()
        | Now x -> at_worker_value x
        | Yield (prio, f) -> write_back (prio, f)
        | Choice (m1, m2) ->
          handle_status m1 write_back;
          handle_status m2 write_back
      in
      Work_datastructure.work_while
        (fun f write_back -> handle_status (Schedulable.run f wls) write_back)
        sched.work_queue

    let spawn_worker sched wls_init ~at_worker_value ~at_worker_init
      ~at_worker_end =
      at_worker_init ();
      Domain.spawn (fun () ->
        Fun.protect ~finally:at_worker_end (fun () ->
          try
            let wls = wls_init () in
            work wls sched
              (at_worker_value ~close_work_queue:(fun () ->
                 Work_datastructure.close sched.work_queue ) )
          with e ->
            let e_s = Printexc.to_string e in
            let bt = Printexc.get_raw_backtrace () in
            let bt_s = Printexc.raw_backtrace_to_string bt in
            let bt_s =
              if String.equal "" bt_s then
                "use OCAMLRUNPARAM=b to get the backtrace"
              else bt_s
            in
            Log.err (fun m ->
              m "a worker ended with exception %s, backtrace is: @\n@[<v>%s@]"
                e_s bt_s );
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

  module Make (Thread : Thread_intf.S) : sig
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

    val assertion_fail :
         Symbolic_boolean.t
      -> Smtml.Model.t
      -> (int * string) list
      -> int list
      -> Symbol_scope.t
      -> 'a t

    val stop : 'a t

    val trap : Result.err -> 'a t

    val thread : thread t

    val yield : Prio.source -> unit t

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
         Symbolic_parameters.Exploration_strategy.t
      -> workers:int
      -> Smtml.Solver_type.t
      -> 'a t
      -> thread
      -> at_worker_value:
           (close_work_queue:(unit -> unit) -> 'a eval * thread -> unit)
      -> at_worker_init:(unit -> unit)
      -> at_worker_end:(unit -> unit)
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

    let solver = lift_schedulable Schedulable.worker_local

    let choose a b =
      (* Here we are doing an optimization: we can clone only one of the two thread because the clone should not have a dependency on the previous state, it is thus safe to re-use it for the other thread. We choose to clone `b` rather than `a` because `a` is likely to be the first one executed. *)
      let* thread in
      let new_thread = Thread.clone thread in
      let b =
        let* () = modify_thread (fun (_ : Thread.t) -> new_thread) in
        b
      in
      State.liftF2 Schedulable.choose a b

    let yield prio = lift_schedulable @@ Schedulable.yield prio

    let stop = lift_schedulable Schedulable.stop

    type 'a run_result = ('a eval * Thread.t) Seq.t

    let run exploration_strategy ~workers solver t thread ~at_worker_value
      ~at_worker_init ~at_worker_end =
      let module M =
        ( val Symbolic_parameters.Exploration_strategy.to_work_ds_module
                exploration_strategy )
      in
      let module Scheduler = Scheduler (M) in
      let open Scheduler in
      let sched = init_scheduler () in
      add_init_task sched (State.run t thread);
      if workers > 1 then Logs_threaded.enable ();
      Array.init workers (fun _i ->
        spawn_worker sched (Solver.fresh solver) ~at_worker_value
          ~at_worker_init ~at_worker_end )

    let trap t =
      let* thread in
      let* solver in
      let path_condition = Thread.pc thread in
      let symbol_scopes = Thread.symbol_scopes thread in
      let stats = Thread.bench_stats thread in
      let* model =
        Benchmark.handle_time_span stats.solver_final_model_time @@ fun () ->
        match Solver.model_of_path_condition solver ~path_condition with
        | Some model -> return model
        | None ->
          (* It can happen when the solver is interrupted *)
          (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
             if solver was interrupted then stop else assert false *)
          stop
      in
      let labels = Thread.labels thread in
      let breadcrumbs = Thread.breadcrumbs thread in
      State.return (ETrap (t, model, labels, breadcrumbs, symbol_scopes))

    let assertion_fail c model labels bcrumbs symbol_scopes =
      State.return (EAssert (c, model, labels, bcrumbs, symbol_scopes))
  end
end

(*
    We can now use CoreImpl only through its exposed signature which
    maintains all invariants.
  *)
module Make (Thread : Thread_intf.S) = struct
  include CoreImpl.Make (Thread)

  let add_pc (c : Symbolic_boolean.t) =
    let c = Symbolic_boolean.to_expr c in
    let c = Smtml.Expr.simplify c in
    match Smtml.Expr.view c with
    | Val True -> return ()
    | Val False -> stop
    | _ ->
      let* thread in
      let c = Symbolic_boolean.of_expr c in
      let new_thread = Thread.add_pc thread c in
      set_thread new_thread
  [@@inline]

  let get_pc () =
    let+ thread in
    let pc = Thread.pc thread in
    let pc = Symbolic_path_condition.slice pc in
    List.fold_left Smtml.Expr.Set.union Smtml.Expr.Set.empty pc

  let add_breadcrumb crumb =
    modify_thread (fun t -> Thread.add_breadcrumb t crumb)

  let add_label label = modify_thread (fun t -> Thread.add_label t label)

  let open_scope scope = modify_thread (fun t -> Thread.open_scope t scope)

  let close_scope () = modify_thread (fun t -> Thread.close_scope t)

  let with_new_invisible_symbol ty f =
    let* thread in
    let n = Thread.num_symbols thread in
    let+ () = modify_thread Thread.incr_num_symbols in
    let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_invisible_%i" n in
    f sym

  let with_new_symbol ty f =
    let* thread in
    let n = Thread.num_symbols thread in
    let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_%d" n in
    let+ () =
      modify_thread (fun thread ->
        let thread = Thread.add_symbol thread sym in
        Thread.incr_num_symbols thread )
    in
    f sym

  (*
    Yielding is currently done each time the solver is about to be called,
    in check_reachability and get_model.
  *)
  let check_reachability v prio =
    let* () = yield prio in
    let* thread in
    let* solver in
    let pc = Thread.pc thread |> Symbolic_path_condition.slice_on_condition v in
    let stats = Thread.bench_stats thread in
    let reachability =
      Benchmark.handle_time_span stats.solver_sat_time @@ fun () ->
      Solver.check solver pc
    in
    return reachability

  let get_model_or_stop symbol =
    (* TODO: better prio here! *)
    let* () = yield Prio.dummy in
    let* solver in
    let* thread in
    let set =
      Thread.pc thread |> Symbolic_path_condition.slice_on_symbol symbol
    in
    let stats = Thread.bench_stats thread in
    let symbol_scopes = Symbol_scope.of_symbol symbol in
    let sat_model =
      Benchmark.handle_time_span stats.solver_intermediate_model_time (fun () ->
        Solver.model_of_set solver ~symbol_scopes ~set )
    in
    match sat_model with
    | `Unsat -> stop
    | `Model model -> begin
      match Smtml.Model.evaluate model symbol with
      | Some v -> return v
      | None ->
        (* the model exists so the symbol should evaluate *)
        assert false
    end
    | `Unknown ->
      (* It can happen when the solver is interrupted *)
      (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
               if solver was interrupted then stop else assert false *)
      stop

  let select_inner ~explore_first ?(with_breadcrumbs = true)
    ~check_only_true_branch (cond : Symbolic_boolean.t) ~prio_true ~prio_false =
    let cond = Symbolic_boolean.to_expr cond in
    let cond = Smtml.Expr.simplify cond in
    match Smtml.Expr.view cond with
    | Val True -> return true
    | Val False -> return false
    | Val (Bitv _bv) -> Fmt.failwith "unreachable (type error)"
    | _ ->
      let is_other_branch_unsat = Atomic.make false in
      let branch condition final_value prio =
        let* () = add_pc condition in
        let* () =
          if with_breadcrumbs then add_breadcrumb (if final_value then 1 else 0)
          else return ()
        in
        (* this is an optimisation under the assumption that the PC is always SAT (i.e. we are performing eager pruning), in such a case, when a branch is unsat, we don't have to check the reachability of the other's branch negation, because it is always going to be SAT. *)
        if Atomic.get is_other_branch_unsat then begin
          Log.debug (fun m ->
            m "The SMT call for the %b branch was optimized away" final_value );
          (* the other branch is unsat, we must be SAT and don't need to check reachability! *)
          return final_value
        end
        else begin
          (* the other branch is SAT (or we haven't computed it yet), so we have to check reachability *)
          let* satisfiability = check_reachability condition prio in
          begin match satisfiability with
          | `Sat -> return final_value
          | `Unsat ->
            Atomic.set is_other_branch_unsat true;
            stop
          | `Unknown ->
            (* It can happen when the solver is interrupted *)
            (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
                   if solver was interrupted then stop else assert false *)
            stop
          end
        end
      in

      let cond = Symbolic_boolean.of_expr cond in

      let true_branch = branch cond true prio_true in

      if check_only_true_branch then true_branch
      else
        let false_branch =
          branch (Symbolic_boolean.not cond) false prio_false
        in
        let* thread in
        Thread.incr_path_count thread;
        if explore_first then choose true_branch false_branch
        else choose false_branch true_branch
  [@@inline]

  let select (cond : Symbolic_boolean.t) ~prio_true ~prio_false =
    select_inner cond ~explore_first:true ~prio_true ~prio_false
      ~check_only_true_branch:false
  [@@inline]

  let summary_symbol (e : Smtml.Expr.t) =
    let* thread in
    match Smtml.Expr.view e with
    | Symbol sym -> return (None, sym)
    | _ ->
      let num_symbols = Thread.num_symbols thread in
      let+ () = modify_thread Thread.incr_num_symbols in
      let sym_name = Fmt.str "choice_i32_%i" num_symbols in
      let sym_type = Smtml.Ty.Ty_bitv 32 in
      let sym = Smtml.Symbol.make sym_type sym_name in
      let assign = Smtml.Expr.(relop Ty_bool Eq (symbol sym) e) in
      (Some assign, sym)

  let select_i32 (i : Symbolic_i32.t) =
    let i = Smtml.Expr.simplify i in
    match Smtml.Expr.view i with
    | Val (Bitv bv) when Smtml.Bitvector.numbits bv <= 32 ->
      return (Smtml.Bitvector.to_int32 bv)
    | _ ->
      let* assign, symbol = summary_symbol i in
      let* () =
        match assign with
        | Some assign ->
          let assign = Symbolic_boolean.of_expr assign in
          add_pc assign
        | None -> return ()
      in
      let rec generator () =
        let* possible_value = get_model_or_stop symbol in
        let i =
          match possible_value with
          | Smtml.Value.Bitv bv when Smtml.Bitvector.numbits bv <= 32 ->
            Smtml.Bitvector.to_int32 bv
          | _ ->
            (* it should be a value! *)
            assert false
        in
        let s = Smtml.Expr.symbol symbol in
        (* TODO: everything which follows look like select_inner and could probably be simplified by calling it directly! *)
        let this_value_cond =
          let open Smtml.Expr in
          Bitv.I32.(s = v i) |> Symbolic_boolean.of_expr
        in
        let not_this_value_cond = Symbolic_boolean.not this_value_cond in
        let this_val_branch =
          let* () = add_breadcrumb (Int32.to_int i) in
          let+ () = add_pc this_value_cond in
          i
        in
        let not_this_val_branch =
          let* () = add_pc not_this_value_cond in
          generator ()
        in
        let* thread in
        Thread.incr_path_count thread;
        choose this_val_branch not_this_val_branch
      in
      generator ()

  let assertion (c : Symbolic_boolean.t) =
    (* TODO: better prio here *)
    let prio_false = Prio.dummy in
    let prio_true = Prio.dummy in
    let* assertion_true =
      select_inner c ~with_breadcrumbs:false ~explore_first:false ~prio_true
        ~prio_false ~check_only_true_branch:false
    in
    if assertion_true then return ()
    else
      let* thread in
      let* solver in
      let path_condition = Thread.pc thread in
      let symbol_scopes = Thread.symbol_scopes thread in
      let stats = Thread.bench_stats thread in
      let* model =
        Benchmark.handle_time_span stats.solver_final_model_time @@ fun () ->
        match Solver.model_of_path_condition solver ~path_condition with
        | Some model -> return model
        | None ->
          (* It can happen when the solver is interrupted *)
          (* TODO: once https://github.com/formalsec/smtml/pull/479 is merged
               if solver was interrupted then stop else assert false *)
          stop
      in
      let breadcrumbs = Thread.breadcrumbs thread in
      let labels = Thread.labels thread in
      assertion_fail c model labels breadcrumbs symbol_scopes

  let ite (c : Symbolic_boolean.t) ~(if_true : Symbolic_value.t)
    ~(if_false : Symbolic_value.t) : Symbolic_value.t t =
    match (if_true, if_false) with
    | I32 if_true, I32 if_false ->
      return (Symbolic_value.I32 (Symbolic_boolean.ite c ~if_true ~if_false))
    | I64 if_true, I64 if_false ->
      return (Symbolic_value.I64 (Symbolic_boolean.ite c ~if_true ~if_false))
    | F32 if_true, F32 if_false ->
      return (Symbolic_value.F32 (Symbolic_boolean.ite c ~if_true ~if_false))
    | F64 if_true, F64 if_false ->
      return (Symbolic_value.F64 (Symbolic_boolean.ite c ~if_true ~if_false))
    | Ref _, Ref _ ->
      (* TODO: better prio here *)
      let prio_false = Prio.dummy in
      let prio_true = Prio.dummy in
      let+ b = select c ~prio_true ~prio_false in
      if b then if_true else if_false
    | _, _ -> assert false

  let depth () =
    let+ thread in
    Thread.depth thread

  let assume c instr_counter =
    (* TODO: better prio here *)
    let* thread in
    let depth = Thread.depth thread in
    let prio_true =
      Prio.v ~depth ~instr_counter ~distance_to_unreachable:None
    in
    let prio_false = Prio.low in
    let* assertion_true =
      select_inner c ~with_breadcrumbs:false ~explore_first:true ~prio_true
        ~prio_false ~check_only_true_branch:true
    in
    if assertion_true then return () else stop
end

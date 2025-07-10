(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Symbolic_value

module Make (Thread : Thread_intf.S) = struct
  type err =
    | Assert_fail
    | Trap of Result.err

  type 'a t =
    | M of (Thread.t -> Solver.t -> ('a, err) Prelude.Result.t * Thread.t)
  [@@unboxed]

  type 'a run_result = ('a, err) Prelude.Result.t * Thread.t

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

  let select (vb : bool) ~prio_t:_ ~prio_f:_ =
    let v = Smtml.Expr.simplify vb in
    match Smtml.Expr.view v with
    | Val True -> return true
    | Val False -> return false
    | _ ->
      Fmt.failwith "Minimalist_symbolic_choice.select failed on %a"
        Smtml.Expr.pp v

  let select_i32 (i : int32) =
    let v = Smtml.Expr.simplify i in
    match Smtml.Expr.view v with
    | Val (Bitv bv) -> return (Smtml.Bitvector.to_int32 bv)
    | _ ->
      Fmt.failwith "Minimalist_symbolic_choice.select_i32 failed on %a"
        Smtml.Expr.pp v

  let trap t = M (fun th _sol -> (Error (Trap t), th))

  let assertion (vb : bool) =
    let v = Smtml.Expr.simplify vb in
    match Smtml.Expr.view v with
    | Val True -> return ()
    | Val False -> M (fun th _sol -> (Error Assert_fail, th))
    | _ ->
      Fmt.failwith "Minimalist_symbolic_choice.assertion failed on %a"
        Smtml.Expr.pp v

  let with_thread f = M (fun st _sol -> (Ok (f st), st))

  let thread = M (fun st _sol -> (Ok st, st))

  let solver = M (fun st sol -> (Ok sol, st))

  let add_pc (_vb : bool) = return ()

  let run ~workers:_ solver t thread = run t thread (Solver.fresh solver ())

  let lift_mem _ = assert false

  let get_pc () = return Smtml.Expr.Set.empty
end

include Make (Thread_with_memory)

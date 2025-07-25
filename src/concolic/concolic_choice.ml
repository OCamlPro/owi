(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type err =
  | Assert_fail
    (* TODO add assertion (will be needed by the environment functions *)
  | Trap of Result.err
  | Assume_fail of Symbolic_value.bool
  | ErrExplicitStop

type pc_elt =
  | Select of Bool.t * Symbolic_value.bool
  | Select_i32 of Int32.t * Symbolic_value.int32
  | Assume of Symbolic_value.bool
  | Assert of Symbolic_value.bool
  | EltExplicitStop

type pc = pc_elt list

type assignments = (Smtml.Symbol.t * Concrete_value.t) list

let pp_assignments ~no_value fmt assignments =
  let open Smtml in
  let pp_v =
    if not no_value then
      Fmt.parens (Fmt.pair ~sep:Fmt.sp Symbol.pp (Fmt.parens Concrete_value.pp))
    else fun fmt (x, _) ->
      let ty = Symbol.type_of x in
      Fmt.parens (Fmt.pair ~sep:Fmt.sp Smtml.Symbol.pp Smtml.Ty.pp) fmt (x, ty)
  in
  Fmt.pf fmt "(model@\n  %a)" (Fmt.vbox (Fmt.list pp_v)) assignments

let pc_elt_to_expr = function
  | Select (v, c) -> Some (if v then c else Smtml.Expr.Bool.not c)
  | Select_i32 (n, c) -> Some Smtml.Expr.Bitv.I32.(c = v n)
  | Assume c -> Some c
  | Assert _ -> None
  | EltExplicitStop ->
    (* Should never be reached because no model should be requested if we stopped *)
    assert false

let pc_to_exprs pc = List.filter_map pc_elt_to_expr pc |> Smtml.Expr.Set.of_list

type shared_thread_info =
  { memories : Symbolic_memory.collection
  ; tables : Symbolic_table.collection
  ; globals : Symbolic_global.collection
  }

type thread =
  { pc : pc
  ; symbols : int
  ; symbols_value : assignments
  ; preallocated_values : (Smtml.Symbol.t, Smtml.Value.t) Hashtbl.t
  ; shared : shared_thread_info
  }

let init_thread preallocated_values shared =
  { symbols = 0; pc = []; symbols_value = []; preallocated_values; shared }

type 'a run_result = ('a, err) Prelude.Result.t * thread

type 'a t = M of (thread -> 'a run_result) [@@unboxed]

let return v = M (fun t -> (Ok v, t)) [@@inline]

let stop = M (fun st -> (Error ErrExplicitStop, st))

let run (M v) st : _ run_result = v st [@@inline]

let bind v f =
  M
    (fun init_s ->
      let v_final, tmp_st = run v init_s in
      match v_final with
      | Ok v_final -> run (f v_final) tmp_st
      | Error _ as e -> (e, tmp_st) )
[@@inline]

let ( let* ) = bind

let map v f =
  let* v in
  return (f v)
[@@inline]

let ( let+ ) = map

let abort =
  M
    (fun st ->
      (Ok (), { st with pc = Assume (Symbolic_value.Bool.const false) :: st.pc }) )

let add_pc ((_c, s) : Concolic_value.bool) =
  M (fun st -> (Ok (), { st with pc = Assume s :: st.pc }))

let add_pc_to_thread (st : thread) c = { st with pc = c :: st.pc }

let no_choice e =
  let v = Smtml.Expr.simplify e in
  not (Smtml.Expr.is_symbolic v)

let select ((c, s) : Concolic_value.bool) ~prio_true:_ ~prio_false:_ =
  let cond = Select (c, s) in
  let no_choice = no_choice s in
  M (fun st -> (Ok c, if no_choice then st else add_pc_to_thread st cond))
[@@inline]

let select_i32 ((c, s) : Concolic_value.int32) =
  let expr = Select_i32 (c, s) in
  let no_choice = no_choice s in
  M (fun st -> (Ok c, if no_choice then st else add_pc_to_thread st expr))
[@@inline]

let assume ((c, s) : Concolic_value.bool) =
  let assume_pc = Assume s in
  if c then M (fun st -> (Ok (), add_pc_to_thread st assume_pc))
  else M (fun st -> (Error (Assume_fail s), st))

let assertion ((c, s) : Concolic_value.bool) =
  let assert_pc = Assert s in
  if c then
    let no_choice = no_choice s in
    M
      (fun st ->
        (Ok (), if no_choice then st else add_pc_to_thread st assert_pc) )
  else M (fun st -> (Error Assert_fail, st))

let trap t = M (fun th -> (Error (Trap t), th))

let with_thread f = M (fun st -> (Ok (f st), st))

let with_new_invisible_symbol (_ty : Smtml.Ty.t) _fn =
  Logs.err (fun m -> m "Not implemented in concolic mode");
  assert false

let with_new_symbol ty f =
  M
    (fun st ->
      let id = st.symbols + 1 in
      let sym = Fmt.kstr (Smtml.Symbol.make ty) "symbol_%d" id in
      let value = Hashtbl.find_opt st.preallocated_values sym in
      let concrete, v = f sym value in
      let st =
        { st with
          symbols = st.symbols + 1
        ; symbols_value = (sym, concrete) :: st.symbols_value
        }
      in
      (Ok v, st) )

let run preallocated_values (M v) : _ run_result =
  let shared =
    { memories = Symbolic_memory.init ()
    ; tables = Symbolic_table.init ()
    ; globals = Symbolic_global.init ()
    }
  in
  v (init_thread preallocated_values shared)

let run' t : _ run_result =
  let preallocated_values = Hashtbl.create 16 in
  run preallocated_values t

let get_pc () = return Smtml.Expr.Set.empty

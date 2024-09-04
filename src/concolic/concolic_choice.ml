(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type err =
  | Assert_fail
    (* TODO add assertion (will be needed by the environment functions *)
  | Trap of Trap.t
  | Assume_fail of Symbolic_value.vbool

type pc_elt =
  | Select of Symbolic_value.vbool * bool
  | Select_i32 of Symbolic_value.int32 * int32
  | Assume of Symbolic_value.vbool
  | Assert of Symbolic_value.vbool

type pc = pc_elt list

type assignments = (Smtml.Symbol.t * Concrete_value.t) list

let pp_pc_elt fmt = function
  | Select (c, v) -> Fmt.pf fmt "Select(%a, %b)" Smtml.Expr.pp c v
  | Select_i32 (c, v) -> Fmt.pf fmt "Select_i32(%a, %li)" Smtml.Expr.pp c v
  | Assume c -> Fmt.pf fmt "Assume(%a)" Smtml.Expr.pp c
  | Assert c -> Fmt.pf fmt "Assert(%a)" Smtml.Expr.pp c

let pp_pc fmt pc = List.iter (fun e -> Fmt.pf fmt "  %a@\n" pp_pc_elt e) pc

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
  | Select (c, v) -> Some (if v then c else Smtml.Expr.Bool.not c)
  | Select_i32 (c, n) -> Some Smtml.Expr.Bitv.I32.(c = v n)
  | Assume c -> Some c
  | Assert _ -> None

let pc_to_exprs pc = List.filter_map pc_elt_to_expr pc

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

let add_pc (c : Concolic_value.V.vbool) =
  M (fun st -> (Ok (), { st with pc = Assume c.symbolic :: st.pc }))

let add_pc_to_thread (st : thread) c = { st with pc = c :: st.pc }

let no_choice e =
  let v = Smtml.Expr.simplify e in
  not (Smtml.Expr.is_symbolic v)

let select (vb : Concolic_value.V.vbool) =
  let r = vb.concrete in
  let cond = Select (vb.symbolic, r) in
  let no_choice = no_choice vb.symbolic in
  M (fun st -> (Ok r, if no_choice then st else add_pc_to_thread st cond))
[@@inline]

let select_i32 (i : Concolic_value.V.int32) =
  let r = i.concrete in
  let expr = Select_i32 (i.symbolic, i.concrete) in
  let no_choice = no_choice i.symbolic in
  M (fun st -> (Ok r, if no_choice then st else add_pc_to_thread st expr))
[@@inline]

let assume (vb : Concolic_value.V.vbool) =
  let assume_pc = Assume vb.symbolic in
  let r = vb.concrete in
  if r then M (fun st -> (Ok (), add_pc_to_thread st assume_pc))
  else M (fun st -> (Error (Assume_fail vb.symbolic), st))

let assertion (vb : Concolic_value.V.vbool) =
  let assert_pc = Assert vb.symbolic in
  let r = vb.concrete in
  if r then
    let no_choice = no_choice vb.symbolic in
    M
      (fun st ->
        (Ok (), if no_choice then st else add_pc_to_thread st assert_pc) )
  else M (fun st -> (Error Assert_fail, st))

let trap t = M (fun th -> (Error (Trap t), th))

let with_thread f = M (fun st -> (Ok (f st), st))

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
  let preallocated_values = Hashtbl.create 0 in
  run preallocated_values t

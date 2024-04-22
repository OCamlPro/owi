type err =
  | Assert_fail (* TODO add assertion (will be needed by the environment functions *)
  | Trap of Trap.t

type thread = {
  pc : Symbolic_value.vbool list;
  symbols : int;
  symbols_value : (Smtml.Symbol.t * Smtml.Expr.t) list
}

let init_thread = {
  symbols = 0;
  pc = [];
  symbols_value = [];
}

type 'a run_result = ('a, err) Stdlib.Result.t * thread

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

let add_pc (st : thread) c = { st with pc = c :: st.pc }

let select (vb : Concolic_value.V.vbool) =
  let r = vb.c in
  let cond =
    if r then vb.s
    else Smtml.Expr.Bool.not vb.s
  in
  M (fun st -> (Ok r, add_pc st cond))
[@@inline]

let select_i32 (i : Concolic_value.V.int32) =
  let r = i.c in
  let expr = Smtml.Expr.Bitv.I32.(i.s = v i.c) in
  M (fun st -> (Ok r, add_pc st expr))
[@@inline]

let trap t = M (fun th -> (Error (Trap t), th))

let with_thread f = M (fun st -> (Ok (f st), st))

let with_new_symbol ty f =
  M (fun st ->
      let id = st.symbols + 1 in
      let sym = Format.kasprintf (Smtml.Symbol.make ty) "symbol_%d" id in
      let expr, v = f sym in
      let st =
        { st with
          symbols = st.symbols + 1;
          symbols_value = (sym, expr) :: st.symbols_value } in
      (Ok v, st))

let run (M v) : _ run_result = v init_thread

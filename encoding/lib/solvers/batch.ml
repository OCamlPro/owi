open Core

type t = { solver : s; pc : Expression.t ref }
and s = Z3_mappings.solver

exception Unknown

let solver_time = ref 0.0
let solver_count = ref 0

let time_call f acc =
  let start = Caml.Sys.time () in
  let ret = f () in
  acc := !acc +. (Caml.Sys.time () -. start);
  ret

let create () =
  { solver = Z3_mappings.mk_solver (); pc = ref (Boolean.mk_val true) }

let interrupt () = Z3_mappings.interrupt ()
let clone (s : t) : t = { s with pc = ref !(s.pc) }

let add (s : t) (e : Expression.t) : unit =
  s.pc := Expression.add_constraint e !(s.pc)

let get_assertions (s : t) : Expression.t = !(s.pc)

let set_default_axioms (s : t) : unit =
  Z3_mappings.add_solver s.solver
    (List.map ~f:Z3_mappings.encode_expr Axioms.axioms)

let check_sat (s : t) (es : Expression.t list) : bool =
  let es' = List.map ~f:Z3_mappings.encode_expr es in
  solver_count := !solver_count + 1;
  let sat = time_call (fun () -> Z3_mappings.check s.solver es') solver_time in
  match sat with
  | Z3.Solver.SATISFIABLE -> true
  | Z3.Solver.UNSATISFIABLE -> false
  | Z3.Solver.UNKNOWN -> raise Unknown

let check (s : t) (expr : Expression.t option) : bool =
  let expression =
    Z3_mappings.encode_expr
      (Option.fold expr ~init:!(s.pc) ~f:(fun f e ->
           Expression.add_constraint e f))
  in
  solver_count := !solver_count + 1;
  let sat =
    time_call (fun () -> Z3_mappings.check s.solver [ expression ]) solver_time
  in
  match sat with
  | Z3.Solver.SATISFIABLE -> true
  | Z3.Solver.UNSATISFIABLE -> false
  | Z3.Solver.UNKNOWN -> raise Unknown

let fork (s : t) (e : Expression.t) : bool * bool =
  (check s (Some e), check s (Some (Expression.negate_relop e)))

let model (s : t) : Z3_mappings.model Option.t = Z3_mappings.get_model s.solver

let eval (s : t) (e : Expression.t) (es : Expression.t list) : Value.t option =
  let es' = List.map ~f:Z3_mappings.encode_expr es in
  ignore (time_call (fun () -> Z3_mappings.check s.solver es') solver_time);
  Option.value_map (model s) ~default:None ~f:(fun m ->
      Z3_mappings.value_of_const m e)

let value_binds ?(symbols : Symbol.t list option) (s : t) :
    (Symbol.t * Value.t) list =
  Option.value_map (model s) ~default:[] ~f:(Z3_mappings.value_binds ?symbols)

let string_binds (s : t) : (string * string * string) list =
  Option.value_map (model s) ~default:[] ~f:Z3_mappings.string_binds

let find_model (s : t) (es : Expression.t list) : (Symbol.t * Value.t) list =
  if check_sat s es then value_binds ~symbols:(Expression.get_symbols es) s
  else []

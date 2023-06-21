open Core

exception Unknown

let solver_time = ref 0.0
let solver_count = ref 0

let time_call f acc =
  let start = Caml.Sys.time () in
  let ret = f () in
  acc := !acc +. (Caml.Sys.time () -. start);
  ret

type s = Z3_mappings.solver
type t = { solver : s; pc : Expression.t ref }

let create () : t =
  { solver = Z3_mappings.mk_solver (); pc = ref (Boolean.mk_val true) }

let interrupt () = Z3_mappings.interrupt ()

let clone (e : t) : t =
  { solver = Z3_mappings.translate e.solver; pc = ref !(e.pc) }

let add (e : t) (c : Expression.t) : unit =
  e.pc := Expression.add_constraint c !(e.pc);
  let ec = Z3_mappings.encode_expr ~bool_to_bv:false c in
  Z3_mappings.add_solver e.solver [ ec ]

let get_assertions (e : t) : Expression.t = !(e.pc)

let check (e : t) (expr : Expression.t option) : bool =
  let expr' =
    Option.to_list
      (Option.map ~f:(Z3_mappings.encode_expr ~bool_to_bv:false) expr)
  in
  let b =
    solver_count := !solver_count + 1;
    let sat =
      time_call (fun () -> Z3_mappings.check e.solver expr') solver_time
    in
    match sat with
    | Z3.Solver.SATISFIABLE -> true
    | Z3.Solver.UNKNOWN -> raise Unknown
    | Z3.Solver.UNSATISFIABLE -> false
  in
  b

let fork (s : t) (e : Expression.t) : bool * bool =
  (check s (Some e), check s (Some (Expression.negate_relop e)))

let model (e : t) : Z3_mappings.model Option.t = Z3_mappings.get_model e.solver

let value_binds ?(symbols : Symbol.t list option) (e : t) :
    (Symbol.t * Value.t) list =
  Option.value_map (model e) ~default:[] ~f:(Z3_mappings.value_binds ?symbols)

let string_binds (e : t) : (string * string * string) list =
  Option.value_map (model e) ~default:[] ~f:Z3_mappings.string_binds

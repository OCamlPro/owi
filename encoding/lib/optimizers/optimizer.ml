open Base

exception Unknown

type t = Z3_mappings.optimize

let solver_time = ref 0.0

let time_call ~f ~accum =
  let start = Caml.Sys.time () in
  let ret = f () in
  accum := !accum +. (Caml.Sys.time () -. start);
  ret

let create () : t = Z3_mappings.mk_opt ()
let push (opt : t) : unit = Z3.Optimize.push opt
let pop (opt : t) : unit = Z3.Optimize.pop opt

let add (opt : t) (es : Expression.t list) : unit =
  Z3_mappings.add_opt opt
    (List.map ~f:(Z3_mappings.encode_expr ~bool_to_bv:false) es)

let check (opt : t) (e : Expression.t) (pc : Expression.t list) target =
  push opt;
  add opt pc;
  ignore (target opt (Z3_mappings.encode_expr ~bool_to_bv:false e));
  ignore (time_call ~f:(fun () -> Z3.Optimize.check opt) ~accum:solver_time);
  let model = Z3_mappings.get_opt_model opt in
  pop opt;
  model

let maximize (opt : t) (e : Expression.t) (pc : Expression.t list) :
    Value.t option =
  let model = check opt e pc Z3_mappings.maximize in
  Option.value_map model ~default:None ~f:(fun m ->
      Z3_mappings.value_of_const m e)

let minimize (opt : t) (e : Expression.t) (pc : Expression.t list) :
    Value.t option =
  let model = check opt e pc Z3_mappings.minimize in
  Option.value_map model ~default:None ~f:(fun m ->
      Z3_mappings.value_of_const m e)

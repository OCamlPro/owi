(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type 'a solver_module = (module Smtml.Solver_intf.S with type t = 'a)

type t = S : ('a solver_module * 'a) -> t [@@unboxed]

let fresh solver () =
  let module Mapping = (val Smtml.Solver_dispatcher.mappings_of_solver solver)
  in
  let module Mapping = Mapping.Fresh.Make () in
  let module Batch = Smtml.Solver.Cached (Mapping) in
  let solver = Batch.create ~logic:QF_BVFP () in
  S ((module Batch), solver)

let check (S (solver_module, s)) pc =
  let module Solver = (val solver_module) in
  Solver.check_set s pc

let model (S (solver_module, s)) ~scoped_symbols ~pc =
  let module Solver = (val solver_module) in
  let symbols = Some (Scoped_symbol.only_symbols scoped_symbols) in
  let model =
    match Solver.get_sat_model ?symbols s pc with
    | `Unknown -> assert false
    | `Unsat ->
      (* Should not happen because we checked just before that is must be true. *)
      Logs.err (fun m -> m "something is wrong... I can not get a model");
      Logs.err (fun m ->
        m "symbols: %a"
          (Fmt.option
             (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ", ") Smtml.Symbol.pp) )
          symbols );
      Logs.err (fun m ->
        m "pc:@\n  @[<v>%a@]"
          (Smtml.Expr.Set.pretty
             ~pp_sep:(fun fmt () -> Fmt.pf fmt " ;@\n")
             Smtml.Expr.pp )
          pc );
      assert false
    | `Model model -> model
  in
  model

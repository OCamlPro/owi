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

let model (S (solver_module, s)) ~symbols ~pc =
  let module Solver = (val solver_module) in
  let model =
    match Solver.check_set s pc with
    | `Sat -> begin
      match Solver.model ?symbols s with
      | None ->
        (* Should not happen because we checked just before that is must be true. *)
        Fmt.epr "symbols: %a@\n"
          (Fmt.option
             (Fmt.list ~sep:(fun fmt () -> Fmt.pf fmt ", ") Smtml.Symbol.pp) )
          symbols;
        Fmt.epr "pc:@\n  @[<v>%a@]@\n"
          (Smtml.Expr.Set.pretty
             ~pp_sep:(fun fmt () -> Fmt.pf fmt " ;@\n")
             Smtml.Expr.pp )
          pc;
        assert false
      | Some model -> model
    end
    | `Unsat -> assert false
    | `Unknown ->
      (* When reached, it means we finally manage to ask the solver something he can not do.
         Please open an issue so we can investigate. *)
      assert false
  in
  model

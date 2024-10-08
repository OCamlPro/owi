(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

open Fmt

type t =
  { annotid : string
  ; items : Sexp.t
  }

type 'a annot =
  | Contract of 'a Contract.t
  | Annot of t

let pp_annot fmt = function
  | Contract contract ->
    pf fmt "(@%a@\n  @[<v>%a@]@\n)" string "contract" Contract.pp_contract
      contract
  | Annot annot ->
    pf fmt "(@%a@\n  @[<hv>%a@]@\n)" string annot.annotid Sexp.pp_sexp
      annot.items

let annot_recorder : (string, Sexp.t) Hashtbl.t = Hashtbl.create 17

let record_annot annotid sexp = Hashtbl.add annot_recorder annotid sexp

let get_annots () =
  let res =
    Hashtbl.fold
      (fun annotid items acc -> { annotid; items } :: acc)
      annot_recorder []
  in
  Hashtbl.reset annot_recorder;
  res

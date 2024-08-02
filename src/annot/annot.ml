open Fmt
open Types

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

let annot_recorder : (string, text annot) Hashtbl.t = Hashtbl.create 17

let record_annot annotid annot = Hashtbl.add annot_recorder annotid annot

let get_annots ?name () =
  match name with
  | Some name -> Hashtbl.find_all annot_recorder name
  | None -> Hashtbl.fold (fun _ annot acc -> annot :: acc) annot_recorder []

open Fmt

type annot =
  { annotid : string
  ; items : Sexp.t
  }

let pp_annot fmt annot =
  pf fmt "(@%a@\n  @[<b 2>%a@]@\n)" string annot.annotid Sexp.pp_sexp
    annot.items

let annot_recorder : (string, annot) Hashtbl.t = Hashtbl.create 17

let record_annot annot = Hashtbl.add annot_recorder annot.annotid annot

let get_annots ?name () =
  match name with
  | Some name -> Hashtbl.find_all annot_recorder name
  | None -> Hashtbl.fold (fun _ annot acc -> annot :: acc) annot_recorder []

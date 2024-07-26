open Fmt

type annot =
  { annotid : string
  ; items : item list
  }

and item =
  | Atom of string
  | String of string
  | Id of string
  | Int of string
  | Float of string
  | Parens of item list
  | Annot of annot

(* TODO: find a better way to format annotations, possibly by
      - recording extra format information when parsing
      - defining rules specific to each sort of annotations *)

let rec pp_annot fmt annot =
  pf fmt "(@%a@\n  @[<b 2>%a@]@\n)" string annot.annotid (list ~sep:sp pp_item)
    annot.items

and pp_item fmt = function
  | Atom atom -> string fmt atom
  | String str -> string fmt str
  | Id id -> Types.pp_id fmt id
  | Int i -> string fmt i
  | Float f -> string fmt f
  | Parens items -> list ~sep:sp pp_item fmt items
  | Annot annot -> pp_annot fmt annot

let annot_recorder : (string, annot) Hashtbl.t = Hashtbl.create 17

let record_annot annot = Hashtbl.add annot_recorder annot.annotid annot

let get_annots ?name () =
  match name with
  | Some name -> Hashtbl.find_all annot_recorder name
  | None -> Hashtbl.fold (fun _ annot acc -> annot :: acc) annot_recorder []

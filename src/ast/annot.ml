open Format

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

module NameMap = Map.Make (String)

(* TODO: find a better way to format annotations, possibly by
      - recording extra format information when parsing
      - defining rules specific to each sort of annotations *)

let rec pp_annot fmt annot =
  pp fmt "(annot %a@\n  @[<b 2>%a@]@\n)" pp_string annot.annotid
    (pp_list ~pp_sep:pp_space pp_item)
    annot.items

and pp_item fmt = function
  | Atom atom -> pp_string fmt atom
  | String str -> pp_string fmt str
  | Id id -> Types.pp_id fmt id
  | Int i -> pp_string fmt i
  | Float f -> pp_string fmt f
  | Parens items -> pp_list ~pp_sep:pp_space pp_item fmt items
  | Annot annot -> pp_annot fmt annot

let annot_recorder : annot list NameMap.t ref = ref NameMap.empty

let find_nil key map = Option.value (NameMap.find_opt key map) ~default:[]

let record_annot annot =
  let old = find_nil annot.annotid !annot_recorder in
  annot_recorder := NameMap.add annot.annotid (annot :: old) !annot_recorder

let get_annots ?name () =
  let annots =
    match name with
    | Some name -> find_nil name !annot_recorder
    | None ->
      List.concat_map
        (fun (_, annots) -> annots)
        (NameMap.to_list !annot_recorder)
  in
  annot_recorder := NameMap.empty;
  annots

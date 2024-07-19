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

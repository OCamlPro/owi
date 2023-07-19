
module IMap = Map.Make (Int)

type table = Sym_value.S.ref_value array

type tables = {
  mutable tables : table IMap.t;
}

let init () = {
  tables = IMap.empty;
}

let clone tables =
  { tables = IMap.map Array.copy tables.tables }

let convert_ref_values (v:Value.ref_value) : Sym_value.S.ref_value =
  match v with
  | Funcref f -> Funcref f
  | _ -> assert false

let convert (orig_table:Table.t) : table =
  Array.map convert_ref_values orig_table.data

let get_table (orig_table:Table.t) (tables : tables) i =
  match IMap.find_opt i tables.tables with
  | Some t -> t
  | None ->
    let t = convert orig_table in
    tables.tables <- IMap.add i t tables.tables;
    t

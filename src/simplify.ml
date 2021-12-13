open Types

(*
type module_ =
  { id : id option
  ; mem : mem option
  ; start : start option
  ; types : type_ Array.t
  ; funcs : func Array.t
  ; tables : table Array.t
  ; globals : global Array.t
  ; elems : elem Array.t
  ; datas : data Array.t
  ; imports : import Array.t
  ; exports : export Array.t
  }
*)

type module_ =
  { fields : module_field list
  ; funcs : func Array.t
  ; seen_funcs : (string, int) Hashtbl.t
  ; exported_funcs : (string, int) Hashtbl.t
  ; memories : (Bytes.t ref * int option) array
  ; tables : (ref_type * indice option Array.t * int option) array
  ; types : func_type Array.t
  }

type action =
  | Invoke_indice of int * string * const list
  | Get of string option * string

type assert_ =
  | SAssert_return of action * result list
  | SAssert_trap of action * failure
  | SAssert_malformed of Types.module_ * failure
  | SAssert_malformed_quote of string list * failure
  | SAssert_malformed_binary of string list * failure
  | SAssert_invalid of Types.module_ * failure
  | SAssert_invalid_quote of string list * failure
  | SAssert_invalid_binary of string list * failure

type cmd =
  | Module_indice of int
  | Assert of assert_
  | Register_indice of string * int
  | Action of action

type script = module_ Array.t * cmd list

let action last_module seen_modules = function
  | Invoke (mod_name, f, args) ->
    let i =
      match mod_name with
      | None -> begin
        match last_module with
        | None -> failwith "no module defined"
        | Some i -> i
      end
      | Some mod_name -> begin
        match Hashtbl.find_opt seen_modules mod_name with
        | None -> failwith @@ Format.sprintf "unknown module $%s" mod_name
        | Some i -> i
      end
    in
    Invoke_indice (i, f, args)
  | Get _ -> failwith "not yet implemented"

let assert_ last_module seen_modules =
  let action = action last_module seen_modules in
  function
  | Assert_return (a, res) -> SAssert_return (action a, res)
  | Assert_trap (a, failure) -> SAssert_trap (action a, failure)
  | Assert_malformed (module_, failure) -> SAssert_malformed (module_, failure)
  | Assert_malformed_quote (m, failure) -> SAssert_malformed_quote (m, failure)
  | Assert_malformed_binary (m, failure) -> SAssert_malformed_binary (m, failure)
  | Assert_invalid (module_, failure) -> SAssert_invalid (module_, failure)
  | Assert_invalid_quote (m, failure) -> SAssert_malformed_quote (m, failure)
  | Assert_invalid_binary (m, failure) -> SAssert_invalid_binary (m, failure)

let mk_module m =
  let fields = m.Types.fields in

  let curr_func = ref (-1) in
  let seen_funcs = Hashtbl.create 512 in
  let exported_funcs = Hashtbl.create 512 in

  let mem_max_size = ref None in
  let mem_bytes = ref (Bytes.create 0) in

  let tables = ref [] in

  let types = Hashtbl.create 512 in
  let curr_type = ref (-1) in
  let seen_types = Hashtbl.create 512 in

  List.iter
    (function
      | MFunc f ->
        incr curr_func;
        let i = !curr_func in
        Option.iter (fun id -> Hashtbl.replace seen_funcs id i) f.id
      | MExport { name; desc } -> begin
        match desc with
        | Export_func indice ->
          let i =
            match indice with
            | Raw i -> Unsigned.UInt32.to_int i
            | Symbolic id -> begin
              match Hashtbl.find_opt seen_funcs id with
              | None ->
                (* TODO: removes this from the parser and add some anonymous case instead *)
                if id = "TODO_func" then
                  !curr_func
                else
                  failwith @@ Format.sprintf "undefined export %s" id
              | Some i -> i
            end
          in
          Hashtbl.replace exported_funcs name i
        | _ -> ()
      end
      | MMem (_id, { min; max }) ->
        mem_max_size := Option.map Unsigned.UInt32.to_int max;
        mem_bytes := Bytes.create (Unsigned.UInt32.to_int min * page_size)
      | MTable (_id, ({ min; max }, rt)) ->
        let tbl =
          ( rt
          , Array.make (Unsigned.UInt32.to_int min) None
          , Option.map Unsigned.UInt32.to_int max )
        in
        tables := tbl :: !tables
      | MType (id, t) -> (
        incr curr_type;
        Hashtbl.add types !curr_type t;
        match id with
        | None -> () (* TODO: is there really nothing to do ? *)
        | Some id -> Hashtbl.add seen_types id !curr_type )
      | _ -> () )
    fields;

  let tables = Array.of_list @@ List.rev !tables in

  let funcs =
    Array.of_list @@ List.rev
    @@ List.fold_left
         (fun acc -> function
           | MFunc f -> f :: acc
           | _field -> acc )
         [] fields
  in

  let funcs =
    Array.map
      (fun f ->
        let local_tbl = Hashtbl.create 512 in
        let type_f =
          match f.type_f with
          | FTId i -> (
            let i =
              match i with
              | Raw i -> Unsigned.UInt32.to_int i
              | Symbolic i -> (
                match Hashtbl.find_opt seen_types i with
                | None -> failwith @@ Format.sprintf "unbound type indice $%s" i
                | Some i -> i )
            in
            match Hashtbl.find_opt types i with
            | None -> failwith @@ Format.sprintf "unbound type indice %d" i
            | Some t -> FTFt t )
          | FTFt _ as t -> t
        in
        let param_n =
          match type_f with
          | FTId _i -> failwith "TODO FTId (simplify)"
          | FTFt (pt, _rt) ->
            List.iteri
              (fun i p ->
                match p with
                | None, _vt -> ()
                | Some id, _vt -> Hashtbl.add local_tbl id i )
              pt;
            List.length pt
        in
        List.iteri
          (fun i l ->
            match l with
            | None, _vt -> ()
            | Some id, _vt -> Hashtbl.add local_tbl id (param_n + i) )
          f.locals;
        let rec body = function
          | Br_if (Symbolic id) -> Br_if (Symbolic id) (* TODO *)
          | Br (Symbolic id) -> Br (Symbolic id) (* TODO *)
          | Call (Symbolic id) -> begin
            match Hashtbl.find_opt seen_funcs id with
            | None -> failwith @@ Format.sprintf "unbound func: %s" id
            | Some i -> Call (Raw (Unsigned.UInt32.of_int i))
          end
          | Local_set (Symbolic id) -> begin
            match Hashtbl.find_opt local_tbl id with
            | None -> failwith @@ Format.sprintf "unbound local: %s" id
            | Some i -> Local_set (Raw (Unsigned.UInt32.of_int i))
          end
          | Local_get (Symbolic id) -> begin
            match Hashtbl.find_opt local_tbl id with
            | None -> failwith @@ Format.sprintf "unbound local: %s" id
            | Some i -> Local_get (Raw (Unsigned.UInt32.of_int i))
          end
          | If_else (bt, e1, e2) -> If_else (bt, expr e1, expr e2)
          | Loop (bt, e) -> Loop (bt, expr e)
          | Block (bt, e) -> Block (bt, expr e)
          | i -> i
        and expr e = List.map body e in
        let body = expr f.body in
        { f with body } )
      funcs
  in

  let types = Hashtbl.to_seq_values types |> Array.of_seq in

  { fields
  ; funcs
  ; seen_funcs = Hashtbl.create 512
  ; exported_funcs
  ; memories = [| (mem_bytes, !mem_max_size) |]
  ; tables
  ; types
  }

let script script =
  let modules =
    Array.of_list @@ List.rev
    @@ List.fold_left
         (fun acc -> function
           | Module m -> mk_module m :: acc
           | Assert _
           | Register _
           | Action _ ->
             acc )
         [] script
  in

  let curr_module = ref (-1) in
  let last_module = ref None in
  let seen_modules = Hashtbl.create 512 in

  let script =
    List.map
      (function
        | Module m ->
          incr curr_module;
          let i = !curr_module in
          last_module := Some i;
          Option.iter (fun id -> Hashtbl.replace seen_modules id i) m.id;
          Module_indice i
        | Assert a -> Assert (assert_ !last_module seen_modules a)
        | Register (name, mod_name) ->
          let i =
            match mod_name with
            | None -> begin
              match !last_module with
              | None -> failwith "no module defined"
              | Some i -> i
            end
            | Some mod_name -> begin
              match Hashtbl.find_opt seen_modules mod_name with
              | None -> failwith @@ Format.sprintf "unknown module $%s" mod_name
              | Some i -> i
            end
          in
          Register_indice (name, i)
        | Action a -> Action (action !last_module seen_modules a) )
      script
  in

  (script, modules)

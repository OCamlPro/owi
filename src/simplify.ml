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

type module_ = { fields : module_field list }

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

let script script =
  let modules =
    Array.of_list @@ List.rev
    @@ List.fold_left
         (fun acc -> function
           | Module m -> { fields = m.fields } :: acc
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

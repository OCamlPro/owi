open Owi.Types.Symbolic
open Crowbar

type t =
  { mutable next_data : int
  ; mutable next_memory : int
  ; mutable next_global : int
  ; mutable next_fun : int
  ; mutable next_local : int
  ; mutable datas : string list
  ; mutable memory : string option
  ; mutable globals : (string * global_type) list
  ; mutable locals : (string * val_type) list
  ; mutable funcs : (string * block_type) list
  ; mutable fuel : int
  }

let empty () =
  { next_data = 0
  ; next_memory = 0
  ; next_global = 0
  ; next_fun = 0
  ; next_local = 0
  ; datas = []
  ; memory = None
  ; globals = []
  ; locals = []
  ; funcs = []
  ; fuel = Param.initial_fuel
  }

let reset_locals env = env.locals <- []

let add_data env =
  let n = env.next_data in
  let name = Format.sprintf "d%d" n in
  env.datas <- name :: env.datas;
  env.next_data <- succ n;
  name

let add_memory env =
  match env.memory with
  | None -> 
    let n = env.next_memory in
    let name = Format.sprintf "m%d" n in
    env.memory <- Some name;
    env.next_memory <- succ n;
    name
  | Some _ -> failwith "a memory already exists"

let add_global env typ =
  let n = env.next_global in
  let name = Format.sprintf "g%d" n in
  env.globals <- (name, typ) :: env.globals;
  env.next_global <- succ n;
  name

let add_local env typ =
  let n = env.next_local in
  let name = Format.sprintf "l%d" n in
  env.locals <- (name, typ) :: env.locals;
  env.next_local <- succ n;
  name

let add_func env typ =
  let n = env.next_fun in
  let name = Format.sprintf "f%d" n in
  env.next_fun <- succ n;
  env.funcs <- (name, typ) :: env.funcs;
  name

let get_globals ntyp env ~only_mut =
  let is_typ global =
    let _, (m, v) = global in
    match v with
    | Num_type nt -> nt = ntyp && (not only_mut || m = Owi.Types.Var)
    | Ref_type _ -> false
  in
  List.filter is_typ env.globals

let get_locals ntyp env =
  let is_typ local =
    let (_, v) = local in
    match v with Num_type nt -> nt = ntyp | Ref_type _ -> false
  in
  List.filter is_typ env.locals

let use_fuel env = env.fuel <- pred env.fuel

let has_fuel env = env.fuel > 0

let has_no_fuel env = not (has_fuel env)

let refill_fuel env = env.fuel <- Param.initial_fuel
